# -*- coding: utf-8 -*-

# Formulář s tiskovým preview a tiskem
# 
# Copyright (C) 2002-2013 Brailcom, o.p.s.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

"""Formulář s tiskovým preview a tiskem.

Tento soubor definuje příslušný formulář a napojení tisku přes wxWindows.
Neřeší tvorbu tiskových sestav jako takových, to je záležitost modulu
'pytis.output'.

Poznámka: Výstižnější než 'lib*output' by bylo 'lib*print', to je však
v konfliktu s klíčovým slovem Pythonu.

"""

import os
import thread


from pytis.form import *
import wx
import lcg
import pytis.output
import pytis.util
import pytis.windows
import config

_ = pytis.util.translations('pytis-wx')

class PostscriptException(Exception):
    """Výjimka vyvolávaná při jakékoliv chybě interpretace PS dat."""
    def __init__(self, *args):
        log(EVENT, 'Chyba interpretace PostScriptu')
        super(PostscriptException, self).__init__(*args)


class _Ghostscript(Tmpdir):
    # Používáme vyrendrování PostScriptu do souborů po stránkách.  To sice
    # není optimální, je to však nejjednodušší, zejména s ohledem na potíže
    # s wxWindows.

    def __init__(self, stream, zoom):
        super(_Ghostscript, self).__init__(prefix='pytisps')
        self._file_pattern = os.path.join(self._tmpdir, '%d')
        self._number_of_pages = None, False # number-of-pages, finished
        self._start_gs(stream, zoom)

    def __del__(self):
        try:
            self._stream.close()
        except IOError:
            pass
        super(_Ghostscript, self).__del__()
        try:
            self._process.wait()
        except OSError:
            pass

    def _start_gs(self, stream, zoom):
        if __debug__: log(DEBUG, 'Start Ghostscriptu:', zoom)
        zoom = 100 * zoom
        gs_command = ('gs -dNOPAUSE -dQUIET -dSAFER -sDEVICE=bmpgray -dTextAlphaBits=4 ' + \
                      '-r%dx%d -sOutputFile=%s -') % \
                      (zoom, zoom, self._file_pattern)
        self._finished = False
        process = self._process = Popen(gs_command, to_child=stream,
                                        from_child=dev_null_stream('w'))
        self._stream = process.to_child()
        if __debug__: log(DEBUG, 'Ghostscript nastartován:', zoom)

    def page_image(self, number):        
        if __debug__: log(DEBUG, 'Stránka od Ghostscriptu:', number)
        try:
            file_name = self._file_pattern % number
            next_file_name = self._file_pattern % (number+1)
            if not os.access(file_name, os.R_OK):
                if __debug__: log(DEBUG, 'Stránka není k dispozici:', number)
                return None
            if not self.finished() and \
                   not os.access(next_file_name, os.R_OK):
                if number == 1:
                    if __debug__:
                        log(DEBUG, 'Čekám na vygenerování první stránky')
                    while not self.finished() and \
                          not os.access(next_file_name, os.R_OK):
                        pass
                    if __debug__: log(DEBUG, 'Čekání ukončeno')
                    if not os.access(file_name, os.R_OK):
                        if __debug__:
                            log(DEBUG, 'První stránka není k dispozici')
                        return None
                else:
                    if __debug__:
                        log(DEBUG, 'Stránka není k dispozici:', number)
                    return None
            if __debug__: log(DEBUG, 'Vracím stránku:', number)
            return wx.Image(file_name)
        except:
            # wxImage nahazuje chybové okno bez ohledu na ošetření výjimky zde
            return None

    def number_of_pages(self):
        number, finished = self._number_of_pages
        if finished:
            return self._number_of_pages
        finished = self.finished()
        if number is None:
            i = 1
        else:
            i = number + 1
        while os.access(self._file_pattern % i, os.R_OK):
            i = i + 1
        self._number_of_pages = i - 1, finished
        return self._number_of_pages

    def finished(self):
        if not self._finished:
            try:
                os.waitpid(self._process.pid(), os.WNOHANG)
            except OSError:
                self._finished = True
        return self._finished

    def input_stream(self):
        return self._stream


class PostscriptViewer(wx.ScrolledWindow):
    """Posuvné okno zobrazující postscriptová data."""
    
    def __init__(self, parent, stream, zoom):
        """Inicializuj prohlížečku.

        Argumenty:

          parent -- rodič prohlížečky, instance 'wx.Window'
          stream -- stream poskytující postscriptová data, která mají být
            zobrazena
          zoom -- kladný integer nebo float odpovídající požadovanému zvětšení
            náhledu, přičemž standardní velikost je 1, menší hodnoty znamenají
            zmenšení, větší hodnoty znamenají zvětšení
                    
        """
        if __debug__: log(DEBUG, 'Startuji PostScriptovou prohlížečku:', zoom)
        # Náhled nelze zoomovat "za běhu", protože jednou nastavené scrollbary
        # ve wxScrolledWindow nelze změnit, museli bychom si udělat
        # scrollování vlastní.
        assert zoom > 0
        wx.ScrolledWindow.__init__(self, parent, -1)
        self._zoom = zoom
        self._gs = None
        self._init_gs(stream)
        self._current_page = None
        self._current_page_bitmap = None
        self._restarted = False
        wx_callback(wx.EVT_PAINT, self, self.OnPaint)

    def _init_gs(self, stream):
        self._gs_old = self._gs
        self._gs = _Ghostscript(stream, self._zoom)
        
    def _wait_for_gs(self):
        if __debug__: log(DEBUG, 'Čekání na dokončení běhu Ghostscriptu')
        while not self._gs.finished():
            time.sleep(1)
        if __debug__: log(DEBUG, 'Běh Ghostscriptu je dokončen')

    def current_page(self):
        """Vrať číslo aktuálně zobrazené stránky, počínaje od 1."""
        return self._current_page

    def number_of_pages(self, current=False):
        """Vrať počet stránek dokumentu.

        Je-li argument 'current' pravdivý, vrať nikoliv počet stránek celého
        dokumentu, nýbrž pouze počet aktuálně vygenerovaných stránek dokumentu.

        Vrací: Dvojici (NUMBER_OF_PAGES, FINAL), kde NUMBER_OF_PAGES je
        odpovídající počet stran a FINAL je pravdivé právě tehdy, je-li
        NUMBER_OF_PAGES číslo konečné.
        
        """
        if not current:
            self._wait_for_gs()
        return self._gs.number_of_pages()
    
    def show_page(self, page_number):
        """Zobraz stránku číslo `page_number'.

        Stránky jsou číslovány od 1.  Jestliže je číslo stránky menší než 1,
        zobraz stranu číslo 1.  Jestliže je číslo stránky větší než počet
        stránek dokumentu, zobraz poslední stránku.

        Vrací: Číslo skutečně zobrazené stránky.

        """
        if __debug__: log(DEBUG, 'Zobrazení stránky:', page_number)
        while True:
            npages, final = self.number_of_pages(current=True)
            if final or npages >= 1:
                if __debug__: log(DEBUG, 'Nějaké stránky už jsou na skladě')
                break
            if __debug__: log(DEBUG, 'mikrospánek')
            microsleep()
        if npages < 1:
            if __debug__: log(DEBUG, 'Výstup nemá žádnou stránku')
            return 0
        gs = self._gs
        if page_number < 1:
            if __debug__: log(DEBUG, 'Vynucené zobrazení první stránky')
            page_number = 1
        elif page_number > npages:
            npages, final = self.number_of_pages()
            if page_number > npages:
                if (not final and
                    self._gs_old is not None and
                    page_number <= self._gs_old.number_of_pages[0]):
                    gs = self._gs_old
                else:
                    page_number = npages
            if __debug__: log(DEBUG, 'Vynucené zobrazení poslední stránky')
        if page_number != self._current_page or self._restarted:
            if gs is self._gs:
                self._restarted = False
            self._current_page = page_number
            image = self._gs.page_image(page_number)
            if image is None:
                if __debug__: log(DEBUG, 'Stránka nenačtena')
                return 0
            self._current_page_bitmap = image.ConvertToBitmap()
            # Scrollbars
            if __debug__: log(DEBUG, 'Nastavuji scrollbars')
            image = self._gs.page_image(1)
            hsteps = vsteps = 100
            if image is None:
                width = height = 100
            else:
                width, height = image.GetWidth(), image.GetHeight()
            hs, vs = width/hsteps, height/vsteps
            if hs and vs:
                # TODO: Bráníme se dělení nulou. Neměli bychom však přesto
                # scrollbary v takovém případě nějak nastravit?
                self.SetScrollbars(hs, vs, width/hs + 1, height/vs + 1)
            self.Refresh()
        if __debug__: log(DEBUG, 'Zobrazena stránka:', page_number)
        return page_number

    def current_zoom(self):
        """Vrať hodnotu zoom z konstruktoru."""
        return self._zoom

    def ps_input_stream(self):
        """Vrať vstupní stream interpretu Postscriptu."""
        return self._gs.input_stream()

    def restart(self, stream):
        """Spusť nové zobrazení dat.

        Argumenty:

          stream -- stejné jako v konstruktoru

        """
        self._restarted = True
        self._init_gs(stream)
        
    # wx metody

    def OnPaint(self, event):
        dc = wx.PaintDC(self)
        self.PrepareDC(dc)
        self.DoDrawing(dc)
        
    def DoDrawing(self, dc):
        bitmap = self._current_page_bitmap
        if bitmap is None:
            return
        dc.BeginDrawing()
        dc.DrawBitmap(bitmap, 0, 0, False)
        dc.EndDrawing()
    

def print_form():
    """Return proper print preview class."""
    return PrintFormExternal

class PrintForm(Form):
    """Common ancestor of both internal and external print previewers."""

    def _run_formatter_process(self, stream, on_background=False):
        result = None
        try:
            if on_background:
                result = thread.start_new_thread(self._run_formatter, (stream,))
            else:
                result = self._run_formatter(stream)
        except lcg.SubstitutionIterator.NotStartedError:
            tbstring = pytis.util.format_traceback()
            log(OPERATIONAL, 'Print exception caught', tbstring)
            run_dialog(Error, _(u"Chybné použití identifikátoru `data' v tiskové sestavě.\n"
                                u"Možná jste místo něj chtěli použít `current_row'?"))
        except UserBreakException:
            pass
        return result

class PrintFormExternal(PrintForm, PopupForm):
    
    def __init__(self, parent, resolver, name, formatter, guardian=None, **kwargs):
        super(PrintFormExternal, self).__init__(parent, resolver, name, guardian=guardian)
        self._formatter = formatter

    def _run_formatter(self, stream):
        import tempfile
        handle, file_name = tempfile.mkstemp()
        f = os.fdopen(handle, 'w')
        try:
            self._formatter.printout(f)
        except:
            os.remove(file_name)
            raise
        return file_name
        
    def _run_viewer(self, file_name):
        run_viewer(file_name)
        
    def show(self):
        pass
    
    def run(self, *args, **kwargs):
        file_name = self._run_formatter_process(None)
        if file_name is None:
            return
        thread.start_new_thread(self._run_viewer, (file_name,))


def run_viewer(file_name):
    """Run PDF viewer on given file.

    Run it on a remote station if requested in configuration and the remote
    station is available.

    Arguments:

      file_name -- name of the file to run the viewer on; string

    """
    import subprocess
    viewer = config.postscript_viewer
    remote = (config.rpc_remote_view and pytis.windows.windows_available())
    try:
        if remote:
            suffix = (os.path.splitext(file_name)[1] or '.pdf')
            try:
                remote_file = pytis.windows.make_temporary_file(suffix=suffix)
            except:
                remote = False
        if remote:
            try:
                f = open(file_name)
                while True:
                    data = f.read(10000000)
                    if not data:
                        break
                    remote_file.write(data)
                f.close()
            finally:
                remote_file.close()
            pytis.windows.launch_file(remote_file.name())
        elif viewer:
            call_args = viewer.split()
            subprocess.call(call_args + [file_name])
        else:
            import mailcap
            match = mailcap.findmatch(mailcap.getcaps(), 'application/pdf')[1]
            if match:
                command = match['view'] % (file_name,)
                os.system(command)
            else:
                run_dialog(Error, _(u"Nenalezen žádný PDF prohlížeč."))
    finally:
        os.remove(file_name)
