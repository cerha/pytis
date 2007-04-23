# -*- coding: iso-8859-2 -*-

# Formul�� s�tiskov�m preview a tiskem
# 
# Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007 Brailcom, o.p.s.
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

"""Formul�� s�tiskov�m preview a tiskem.

Tento soubor definuje p��slu�n� formul�� a napojen� tisku p�es wxWindows.
Ne�e�� tvorbu tiskov�ch sestav jako takov�ch, to je z�le�itost modulu
'pytis.output'.

Pozn�mka: V�sti�n�j�� ne� 'lib*output' by bylo 'lib*print', to je v�ak
v�konfliktu s�kl��ov�m slovem Pythonu.

"""

import cStringIO
import os
import thread


from pytis.form import *
import wx
import pytis.output
import config


class PostscriptException(Exception):
    """V�jimka vyvol�van� p�i jak�koliv chyb� interpretace PS dat."""
    def __init__(self, *args):
        log(EVENT, 'Chyba interpretace PostScriptu')
        super(PostscriptException, self).__init__(*args)


class _Ghostscript(Tmpdir):
    # Pou��v�me vyrendrov�n� PostScriptu do soubor� po str�nk�ch.  To sice
    # nen� optim�ln�, je to v�ak nejjednodu���, zejm�na s�ohledem na pot�e
    # s�wxWindows.

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
        if __debug__: log(DEBUG, 'Ghostscript nastartov�n:', zoom)

    def page_image(self, number):        
        if __debug__: log(DEBUG, 'Str�nka od Ghostscriptu:', number)
        try:
            file_name = self._file_pattern % number
            next_file_name = self._file_pattern % (number+1)
            if not os.access(file_name, os.R_OK):
                if __debug__: log(DEBUG, 'Str�nka nen� k�dispozici:', number)
                return None
            if not self.finished() and \
                   not os.access(next_file_name, os.R_OK):
                if number == 1:
                    if __debug__:
                        log(DEBUG, '�ek�m na vygenerov�n� prvn� str�nky')
                    while not self.finished() and \
                          not os.access(next_file_name, os.R_OK):
                        pass
                    if __debug__: log(DEBUG, '�ek�n� ukon�eno')
                    if not os.access(file_name, os.R_OK):
                        if __debug__:
                            log(DEBUG, 'Prvn� str�nka nen� k�dispozici')
                        return None
                else:
                    if __debug__:
                        log(DEBUG, 'Str�nka nen� k�dispozici:', number)
                    return None
            if __debug__: log(DEBUG, 'Vrac�m str�nku:', number)
            return wx.Image(file_name)
        except:
            # wxImage nahazuje chybov� okno bez ohledu na o�et�en� v�jimky zde
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
    """Posuvn� okno zobrazuj�c� postscriptov� data."""
    
    def __init__(self, parent, stream, zoom):
        """Inicializuj prohl�e�ku.

        Argumenty:

          parent -- rodi� prohl�e�ky, instance 'wx.Window'
          stream -- stream poskytuj�c� postscriptov� data, kter� maj� b�t
            zobrazena
          zoom -- kladn� integer nebo float odpov�daj�c� po�adovan�mu zv�t�en�
            n�hledu, p�i�em� standardn� velikost je 1, men�� hodnoty znamenaj�
            zmen�en�, v�t�� hodnoty znamenaj� zv�t�en�
                    
        """
        if __debug__: log(DEBUG, 'Startuji PostScriptovou prohl�e�ku:', zoom)
        # N�hled nelze zoomovat "za b�hu", proto�e jednou nastaven� scrollbary
        # ve wxScrolledWindow nelze zm�nit, museli bychom si ud�lat
        # scrollov�n� vlastn�.
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
        if __debug__: log(DEBUG, '�ek�n� na dokon�en� b�hu Ghostscriptu')
        while not self._gs.finished():
            time.sleep(1)
        if __debug__: log(DEBUG, 'B�h Ghostscriptu je dokon�en')

    def current_page(self):
        """Vra� ��slo aktu�ln� zobrazen� str�nky, po��naje od�1."""
        return self._current_page

    def number_of_pages(self, current=False):
        """Vra� po�et str�nek dokumentu.

        Je-li argument 'current' pravdiv�, vra� nikoliv po�et str�nek cel�ho
        dokumentu, n�br� pouze po�et aktu�ln� vygenerovan�ch str�nek dokumentu.

        Vrac�: Dvojici (NUMBER_OF_PAGES, FINAL), kde NUMBER_OF_PAGES je
        odpov�daj�c� po�et stran a FINAL je pravdiv� pr�v� tehdy, je-li
        NUMBER_OF_PAGES ��slo kone�n�.
        
        """
        if not current:
            self._wait_for_gs()
        return self._gs.number_of_pages()
    
    def show_page(self, page_number):
        """Zobraz str�nku ��slo `page_number'.

        Str�nky jsou ��slov�ny od�1.  Jestli�e je ��slo str�nky men�� ne��1,
        zobraz stranu ��slo�1.  Jestli�e je ��slo str�nky v�t�� ne� po�et
        str�nek dokumentu, zobraz posledn� str�nku.

        Vrac�: ��slo skute�n� zobrazen� str�nky.

        """
        if __debug__: log(DEBUG, 'Zobrazen� str�nky:', page_number)
        while True:
            npages, final = self.number_of_pages(current=True)
            if final or npages >= 1:
                if __debug__: log(DEBUG, 'N�jak� str�nky u� jsou na sklad�')
                break
            if __debug__: log(DEBUG, 'mikrosp�nek')
            microsleep()
        if npages < 1:
            if __debug__: log(DEBUG, 'V�stup nem� ��dnou str�nku')
            return 0
        gs = self._gs
        if page_number < 1:
            if __debug__: log(DEBUG, 'Vynucen� zobrazen� prvn� str�nky')
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
            if __debug__: log(DEBUG, 'Vynucen� zobrazen� posledn� str�nky')
        if page_number != self._current_page or self._restarted:
            if gs is self._gs:
                self._restarted = False
            self._current_page = page_number
            image = self._gs.page_image(page_number)
            if image is None:
                if __debug__: log(DEBUG, 'Str�nka nena�tena')
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
                # TODO: Br�n�me se d�len� nulou. Nem�li bychom v�ak p�esto
                # scrollbary v takov�m p��pad� n�jak nastravit?
                self.SetScrollbars(hs, vs, width/hs + 1, height/vs + 1)
            self.Refresh()
        if __debug__: log(DEBUG, 'Zobrazena str�nka:', page_number)
        return page_number

    def current_zoom(self):
        """Vra� hodnotu zoom z�konstruktoru."""
        return self._zoom

    def ps_input_stream(self):
        """Vra� vstupn� stream interpretu Postscriptu."""
        return self._gs.input_stream()

    def restart(self, stream):
        """Spus� nov� zobrazen� dat.

        Argumenty:

          stream -- stejn� jako v�konstruktoru

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
    """Formul�� zobrazuj�c� preview tisku s�mo�nost� jeho proveden�."""
    DESCR = "tiskov� sestava"
    
    def __init__(self, parent, resolver, name, formatter,
                 guardian=None):
        """Inicializuj instanci.

        Argumenty:

          parent -- instance 'wxFrame', do kter�ho formul�� pat��
          resolver -- resolver jmenn�ch odkaz�, instance t��dy
            'pytis.util.Resolver'
          name -- jm�no specifika�n�ho souboru pro resolver; string
          formatter -- form�tova� v�stupu, instance t��dy
            'pytis.output.Formatter'
          guardian -- formul�� (instance libovoln� t��dy), ve kter�m je
            formul�� vlo�en z�hlediska struktury aplikace; nen�-li zad�n, je
            pou�it 'parent'.  Tento parametr je vyu��v�n nap��klad p�i zas�l�n�
            kl�vesov�ch ud�lost� \"nahoru\".  Typicky je to formul��, kter�
            tuto instanci vytv���.

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
        if __debug__: log(DEBUG, 'Spou�t�m form�tova�')
        try:
            self._formatter_running = True
            self._formatter.printout(stream)
            self._number_of_runs = self._number_of_runs + 1
            self._formatter_running = False
        finally:
            if __debug__: log(DEBUG, 'Konec form�tov�n�')

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
        if __debug__: log(DEBUG, 'Zobrazuji prvn� str�nku')
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
        # Kl�vesy
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
        if __debug__: log(DEBUG, 'Po�adavek na zobrazen� str�nky:', page_number)
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
                message(_("V�stup zform�tov�n"))

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
        if __debug__: log(DEBUG, 'Po�adavek na zoom:', zoom)
        if zoom < 10:
            zoom = 10
        elif zoom > 300:                # v�c u� d�v� po��ta�i p��li� zahulit
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
        # Nov� n�hled
        if __debug__: log(DEBUG, 'Restart n�hledu')
        self._start_postscript_viewer(zoom/100.0)
        self._preview.show_page(1)
        sizer.Prepend(self._preview, 1, wx.EXPAND|wx.FIXED_MINSIZE)
        self.SetSize(self.GetSize())
        self._show_page(page)
        if __debug__: log(DEBUG, 'N�hled restartov�n')
        
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
        log(ACTION, "Spu�t�n tisk:", self._name)
        process = Popen(config.printing_command,
                        from_child=dev_null_stream('w'))
        stream = process.to_child()
        thread.start_new_thread(self._run_formatter, (stream,))
        message(_("Spu�t�n tisk"))

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
    
    def __init__(self, parent, resolver, name, formatter, guardian=None):
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
        viewer = config.postscript_viewer
        try:
            # For unknown reasons os.spawnlp ignores command arguments, so
            # let's use os.system instead.
            os.system('%s %s' % (config.postscript_viewer, file_name,))
        finally:
            os.remove(file_name)
        
    def show(self):
        pass
    
    def run(self):
        file_name = self._run_formatter()
        thread.start_new_thread(self._run_viewer, (file_name,))
