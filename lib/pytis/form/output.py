# -*- coding: utf-8 -*-

# Formulář s tiskovým preview a tiskem
# 
# Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2011 Brailcom, o.p.s.
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

import cStringIO
import os
import thread


from pytis.form import *
import wx
import pytis.output
import config


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
        gs_command = ('gs -dNOPAUSE -dQUIET -dSAFER -sDEVICE=bmpmono ' + \
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
        self._gs = gs = _Ghostscript(stream, self._zoom)
        
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
    if config.postscript_viewer:
        form = PrintFormExternal
    else:
        form = PrintFormInternal
    return form

class PrintForm(Form):
    """Common ancestor of both internal and external print previewers."""

class PrintFormInternal(PrintForm, InnerForm):
    """Formulář zobrazující preview tisku s možností jeho provedení."""
    DESCR = "tisková sestava"
    
    def __init__(self, parent, resolver, name, formatter,
                 guardian=None, **kwargs):
        """Inicializuj instanci.

        Argumenty:

          parent -- instance 'wxFrame', do kterého formulář patří
          resolver -- resolver jmenných odkazů, instance třídy
            'pytis.util.Resolver'
          name -- jméno specifikačního souboru pro resolver; string
          formatter -- formátovač výstupu, instance třídy
            'pytis.output.Formatter'
          guardian -- formulář (instance libovolné třídy), ve kterém je
            formulář vložen z hlediska struktury aplikace; není-li zadán, je
            použit 'parent'.  Tento parametr je využíván například při zasílání
            klávesových událostí \"nahoru\".  Typicky je to formulář, který
            tuto instanci vytváří.

        """
        Form.__init__(self, parent, resolver, name, guardian=guardian)
        self._formatter = formatter
        self._current_stream = None
        self._total_pages = None
        self._number_of_runs = 0
        self._formatter_running = False
        self._create_controls()
        wx_callback(wx.EVT_IDLE, self, self._on_idle)

    def _run_formatter(self, stream):
        if __debug__: log(DEBUG, 'Spouštím formátovač')
        try:
            self._formatter_running = True
            self._formatter.printout(stream)
            self._number_of_runs = self._number_of_runs + 1
            self._formatter_running = False
        finally:
            if __debug__: log(DEBUG, 'Konec formátování')

    def _start_postscript_viewer(self, zoom, new=True):
        if new:
            self._preview = PostscriptViewer(self, None, zoom)
        preview = self._preview
        if not new:
            preview.restart(None)
        stream = self._current_stream = preview.ps_input_stream()
        thread.start_new_thread(self._run_formatter, (stream,))
        
    def _create_controls(self):
        self._start_postscript_viewer(1.0)
        preview = self._preview
        if __debug__: log(DEBUG, 'Zobrazuji první stránku')
        preview.show_page(1)
        # Control widgets
        control_sizer = wx.BoxSizer(wx.HORIZONTAL)
        def make_button(callback, label="", id=-1):
            button = wx.Button(self, id, label)
            #button.SetSize(dlg2px(button, 4*(len(label)+2), 15))
            wx_callback(wx.EVT_BUTTON, self, button.GetId(), callback)
            control_sizer.Add(button)
        def make_widget(class_, third_arg, callback=None, fixed=False,
                        style=0, size=3):
            widget = class_(self, -1, third_arg, style=style)
            widget.SetSize(dlg2px(widget, 4*size, 15))
            if callback:
                wx_callback(callback[0], self, widget.GetId(), callback[1])
            control_sizer.Add(widget)
            return widget
        make_button(self._on_first_page, "|<<")
        make_button(self._on_previous_page, "<")
        page_indicator = self._page_indicator_ctrl = \
          make_widget(wx.TextCtrl, '1' , size=7,
                      callback=(wx.EVT_TEXT_ENTER, self._on_goto_page))
        make_widget(wx.StaticText, ' / ', fixed=True)
        npages, final = preview.number_of_pages(current=True)
        npages_string = '%d' % npages
        if final:
            self._total_pages = npages
        total_pages = self._total_pages_ctrl = \
          make_widget(wx.TextCtrl, npages_string, style=wx.TE_READONLY, size=7)

        make_button(self._on_next_page, ">")
        make_button(self._on_last_page, ">>|")
        make_widget(wx.StaticText, ' ', size=8)
        
        zoom = self._zoom_ctrl = \
          make_widget(wx.TextCtrl, '100',
                      callback=(wx.EVT_TEXT_ENTER, self._on_zoom), size=5)
        make_widget(wx.StaticText, ' % ', fixed=True)
        make_button(self._on_zoom_out, id=wx.ID_ZOOM_OUT)
        make_button(self._on_zoom_in_, id=wx.ID_ZOOM_IN)

        make_widget(wx.StaticText, ' ', size=8)
        make_button(self._on_print_, id=wx.ID_PRINT)
        # Sizers
        wx_callback(wx.EVT_SIZE, self, self._on_size)
        sizer = self._sizer = wx.BoxSizer(wx.VERTICAL)
        sizer.Add(preview, 1, wx.EXPAND)
        sizer.Add(control_sizer, 0, wx.EXPAND|wx.FIXED_MINSIZE)
        self.SetSizer(sizer)
        sizer.Fit(self)
        # Klávesy
        wx_callback(wx.EVT_KEY_DOWN, preview, self.on_key_down)
        wx_callback(wx.EVT_KEY_DOWN, self, self.on_key_down)
        wx_callback(wx.EVT_KEY_DOWN, self._page_indicator_ctrl,
                    lambda event, self=self: \
                    self._on_widget_key(event, self._page_indicator_ctrl))
        wx_callback(wx.EVT_KEY_DOWN, self._zoom_ctrl,
                    lambda event, self=self: \
                    self._on_widget_key(event, self._zoom_ctrl))

    def _on_idle(self, event):
        if self._total_pages is None:
            self._show_page(self._preview.current_page())
            microsleep()
        if not self._formatter_running and self._number_of_runs < 2:
            self._start_postscript_viewer(self._zoom, new=False)

    def _on_widget_key(self, event, widget):
        if WxKey().is_event_of_key(event, 'Enter'):
            if widget is self._page_indicator_ctrl:
                self._on_goto_page(event)
            elif widget is self._zoom_ctrl:
                self._on_zoom(event)
            else:
                event.Skip()
        else:
            event.Skip()

    def _show_page(self, page_number):
        if __debug__: log(DEBUG, 'Požadavek na zobrazení stránky:', page_number)
        preview = self._preview
        current_page = preview.current_page()
        real_page_number = preview.show_page(page_number)
        if (real_page_number == current_page and
            real_page_number != page_number):
            beep()
        self._page_indicator_ctrl.SetValue('%d' % real_page_number)
        if self._total_pages is None:
            npages, final = preview.number_of_pages(current=True)
            npages_string = '%d' % npages
            self._total_pages_ctrl.SetValue(npages_string)
            if final:
                self._total_pages = npages
                message(_(u"Výstup zformátován"))

    def _on_goto_page(self, event):
        page_number_string = self._page_indicator_ctrl.GetValue()
        try:
            page_number = int(page_number_string)
        except:
            page_number_string = '%d' % self._preview.current_page()
            self._page_indicator_ctrl.SetValue(page_number_string)
            return
        self._show_page(page_number)

    def _on_first_page(self, event=None):
        self._show_page(1)

    def _on_last_page(self, event=None):
        self._show_page(self._preview.number_of_pages()[0])

    def _on_previous_page(self, event=None):
        self._show_page(self._preview.current_page() - 1)
        
    def _on_next_page(self, event=None):
        self._show_page(self._preview.current_page() + 1)

    def _zoom(self, zoom):
        if __debug__: log(DEBUG, 'Požadavek na zoom:', zoom)
        if zoom < 10:
            zoom = 10
        elif zoom > 300:                # víc už dává počítači příliš zahulit
            zoom = 300
        self._zoom_ctrl.SetValue("%d" % zoom)
        preview = self._preview
        page = preview.current_page()
        sizer = self._sizer
        sizer.Remove(preview)
        preview.Destroy()
        try:
            self._current_stream.close()
        except IOError:
            pass
        # Nový náhled
        if __debug__: log(DEBUG, 'Restart náhledu')
        self._start_postscript_viewer(zoom/100.0)
        self._preview.show_page(1)
        sizer.Prepend(self._preview, 1, wx.EXPAND|wx.FIXED_MINSIZE)
        self.SetSize(self.GetSize())
        self._show_page(page)
        if __debug__: log(DEBUG, 'Náhled restartován')
        
    def _on_zoom(self, event):
        zoom_string = self._zoom_ctrl.GetValue()
        try:
            zoom = int(zoom_string)
        except:
            zoom_string = '%d' % (self._preview.current_zoom() * 100)
            self._zoom_ctrl.SetValue(zoom_string)
            return
        self._zoom(zoom)

    def _on_zoom_in_(self, event):
        zoom = int(self._preview.current_zoom() * 100 * 1.5)
        self._zoom(zoom)

    def _on_zoom_out(self, event):
        zoom = int(self._preview.current_zoom() * 100 * 0.67)
        self._zoom(zoom)

    def _on_print_(self, __event=None):
        log(ACTION, "Spuštěn tisk:", self._name)
        process = Popen(config.printing_command,
                        from_child=dev_null_stream('w'))
        stream = process.to_child()
        thread.start_new_thread(self._run_formatter, (stream,))
        message(_(u"Spuštěn tisk"))

    def _on_size(self, event):
        size = event.GetSize()
        self.SetSize(event.GetSize())
        event.Skip()
        
    def on_command(self, command, **kwargs):
        if command == self.COMMAND_PRINT:
            self._on_print_()
        elif command == self.COMMAND_NEXT_PAGE:
            self._on_next_page()
        elif command == self.COMMAND_PREVIOUS_PAGE:
            self._on_previous_page()
        else:
            return super_(PrintFormInternal).on_command(self, command, **kwargs)
        return True

    def _cleanup(self):
        super(PrintFormInternal, self)._cleanup()
        self._formatter.close()
        if self._current_stream is not None:
            try:
                self._current_stream.close()
            except IOError:
                pass

class PrintFormExternal(PrintForm, PopupForm):
    
    def __init__(self, parent, resolver, name, formatter, guardian=None, **kwargs):
        super(PrintFormExternal, self).__init__(parent, resolver, name, guardian=guardian)
        self._formatter = formatter

    def _run_formatter(self):
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
        import subprocess
        viewer = config.postscript_viewer
        call_args = viewer.split()
        try:
            subprocess.call(call_args + [file_name])
        finally:
            os.remove(file_name)
        
    def show(self):
        pass
    
    def run(self, *args, **kwargs):
        self._run_formatter()
        # Run it once again to make correct total page count in the document
        file_name = self._run_formatter()
        thread.start_new_thread(self._run_viewer, (file_name,))
