# Pøíznak cachování specifikací pøi startu aplikace.
#cache_spec_onstart = True

# Port databázového serveru.
#dbport = None

# Pøíznak zobrazování úvodního uvítacího dialogu.
#show_splash = True

# Barva møí¾ky tabulkového formuláøe. Barva je reprezentována øetìzcem
# '#RRGGBB'.
#grid_line_color = '#6482be'

# Barva záhlaví tabulky pøi zapnutém filtrování. Barva je reprezentována
# øetìzcem '#RRGGBB'.
#filter_color = '#82c882'

# E-mailová adresa, na kterou mají být posílána oznámení o chybì.
#bug_report_address = ''

# Cesta k souboru s logem. Mù¾e být zadán absolutnì i relativnì vzhledem
# k aktuálnímu adresáøi.
#logo = './icons/logo.bmp'

# Shellový pøíkaz sendmail vèetnì celé cesty.
#sendmail_command = '/usr/lib/sendmail'

# Poèet øádkù, které se pøinaèítají do cache pøi dal¹ích selectech z datového
# objektu.
#fetch_size = 100

# Numeric locale. Hodnota musí být string reprezentující locale pro formátování
# èíselných polo¾ek.
#lc_numeric = 'C'

# Barva pozadí needitovatelného vstupního políèka. Barva je reprezentována
# øetìzcem '#RRGGBB'.
#field_disabled_color = '#c0c0c0'

# Jméno tabulky, do které mají být logovány DML SQL pøíkazy.
#dblogtable = ''

# Poèet øádkù, které se pøednaètou do cache pøi prvním selectu z datového
# objektu.
#initial_fetch_size = 100

# Pøíznak vyvolání debuggeru pøi chybì. Dojde-li k odchycení neoèekávané
# výjimky a tato volba je zapnuta, je vyvolán interaktivní debugger.  Je-li
# zapnuta volba 'debug', je implicitnì zapnuta i tato volba.  U¾iteèné pouze
# pro ladìní.
#debug_on_error = False

# Kódování exportovaných dat. Hodnota musí být jedním z podporovaných kódování
# v Pythonu.
#export_encoding = 'iso8859-2'

# Adresáø s obrázkovými soubory. Mù¾e být zadán absolutnì i relativnì vzhledem
# k aktuálnímu adresáøi.
#icon_dir = './icons'

# Sekvence jmen tøíd, jejich¾ debugovací hlá¹ky jsou propu¹tìny. Debugovací
# logovací hlá¹ky ostatních tøíd jsou odfiltrovány. Je-li 'None', jsou
# propu¹tìny v¹echny hlá¹ky (nestanoví-li jiný filtr jinak).  U¾iteèné pouze
# pro ladìní.
#log_class_filter = ('pytis.data.DBDefaultClass',)

# Barva pozadí editovaného øádku. Barva je reprezentována øetìzcem '#RRGGBB'.
#row_edit_bg_color = '#c80000'

# Subject mailu oznámení o chybì aplikace.
#bug_report_subject = 'Bug report: Unexpected exception'

# U¾ivatelské jméno (login) pro databázové spojení.
#dbuser = getpass.getuser()

# Barva pozadí aktivního øádku tabulkového formuláøe. Pokud barva není
# nastavena, bude pou¾ita systémová barva zvýraznìní.  Barva je reprezentována
# øetìzcem '#RRGGBB'.
#row_focus_bg_color = None

# Pozadí políèka needitovatelného kvùli pøístupovým právùm. Barva je
# reprezentována øetìzcem '#RRGGBB'.
#field_inaccessible_color = '#e0e4f0'

# Pøíznak profilování. Je-li zapnut, aplikace se spustí v profilovacím re¾imu a
# ukládá informace o trvání jednotlivých volání do souboru. Zapnutí této volby
# velmi výraznì zpomaluje bìh aplikace.
#profile = False

# Adresáø obsahující definièní soubory. Adresáø mù¾e být zadán absolutnì
# i relativnì vzhledem k aktuálnímu adresáøi.
#def_dir = './defs'

# Adresáø pro doèasné pomocné soubory.
#tmp_dir = '/tmp'

# Barva textu neaktivního øádku tabulkového formuláøe. Barva je reprezentována
# øetìzcem '#RRGGBB'.
#row_nofocus_fg_color = '#000000'

# Pøíznak ladícího re¾imu. Je-li zapnut, aplikace mù¾e bì¾et s více kontrolami
# a vypisovat spoustu informací, obvykle v¹ak za cenu svého výrazného
# zpomalení.
#debug = False

# Specifikace logovací tøídy. Trojice (CLASS, ARGS, KWARGS), kde CLASS je
# logovací tøída a ARGS, resp. KWARGS, jsou argumenty, resp. klíèované
# argumenty, jejího konstruktoru.  Standardní dostupné tøídy jsou SyslogLogger
# a StreamLogger.  Více o nich lze nalézt v jejich dokumentaci.
#log_logger = (log.StreamLogger, (sys.stderr,), {})

# Adresáø pro export do CSV souborù. Hodnota udává cestu k adresáøi, kde se
# budou ukládat textové CSV soubory.
#export_directory = '/tmp'

# Heslo pro pøihlá¹ení k databázi.
#dbpass = None

# Formát èasu. Øetìzec ve tvaru vy¾adovaném parametrem `format' konstruktoru
# tøídy 'pytis.data.Time'.
#time_format = '%H:%M:%S'

# Adresáø obsahující soubory s nápovìdou. Mù¾e být zadán absolutnì i relativnì
# vzhledem k aktuálnímu adresáøi.
#help_dir = './help'

# Seznam formuláøù, které mají být otevøeny po spu¹tìní aplikace.
#startup_forms = None

# Pøíznak výpisu ladících informací o pamìti. Je-li zapnuta, aplikace vypisuje
# informativní hlá¹ky garbage collectoru a jiné údaje o pamìti.
#debug_memory = False

# Urèuje, zda je preferováno struèné nebo jednotné formátování. Je-li tato
# volba nastavena na pravdu, jsou krátká data v logovacích hlá¹kách
# doporuèujících struènost pøipojena ihned za hlá¹ku místo vypsání na
# samostatný øádek.
#log_one_line_preferred = True

# Ztmavení barvy skupiny pøi seskupování øádkù. Proto¾e barva pozadí øádkù není
# v¾dy bílá, je tato hodnota chápána jako relativní.  O kolik je zvolená barva
# tmav¹í ne¾ bílá, o tolik bude výsledná barva skupiny tmav¹í, ne¾ barva pozadí
# ostatních øádkù.  Barva je reprezentována øetìzcem '#RRGGBB'.
#grouping_background_downgrade = '#eceef0'

# Jméno aplikaèní databáze.
#dbname = 'pytis'

# Adresáø obsahující dokumentaci.
#doc_dir = './docs'

# Formát data. Øetìzec ve tvaru vy¾adovaném parametrem `format' konstruktoru
# tøídy 'pytis.data.Date'.
#date_format = '%Y-%m-%d'

# Seznam typù logovacích hlá¹ek, které mají být odfiltrovány. V seznamu lze
# pou¾ít konstanty 'OPERATIONAL', 'ACTION', 'EVENT' a 'DEBUG'.
#log_exclude = [DEBUG]

# Prefix jména modulu, jeho¾ debugovací hlá¹ky jsou propu¹tìny. Debugovací
# logovací hlá¹ky modulù s jiným prefixem jsou odfiltrovány.  Není-li
# definováno, jsou propu¹tìny v¹echny hlá¹ky (nestanoví-li jiný filtr jinak).
# U¾iteèné pouze pro ladìní.
#log_module_filter = 'pytis.data'

# Formát spoleènì uvedeného data a èasu. Øetìzec ve tvaru vy¾adovaném
# parametrem `format' konstruktoru tøídy 'pytis.data.DateTime'.
#date_time_format = '%Y-%m-%d %H:%M:%S'

# Roztahovat sloupce tabulek, aby vyu¾ily celou ¹íøku okna.
#stretch_tables = True

# Shellový pøíkaz pro provedení tisku, vèetnì argumentù. Pøíkaz musí být
# schopen pøevzít tisková data ze standardního vstupu.
#printing_command = 'lpr'

# Jméno databázového serveru.
#dbhost = 'localhost'

# Barva textu aktivního øádku tabulkového formuláøe. Barva je reprezentována
# øetìzcem '#RRGGBB'.
#row_focus_fg_color = '#ffffff'

# Barva textu editovaného øádku tabulkového formuláøe. Barva je reprezentována
# øetìzcem '#RRGGBB'.
#row_edit_fg_color = '#ffffff'

# Barva pozadí neaktivního øádku tabulkového formuláøe. Barva je reprezentována
# øetìzcem '#RRGGBB'.
#row_nofocus_bg_color = '#b6b6b6'

# Pøíznak zobrazování bublinové nápovìdy.
#show_tooltips = True

# Jméno aplikace. Jméno mù¾e být libovolné, pou¾ívá se napø. jako titulek okna
# nebo pøi logování.  Od nìho je také odvozeno jméno výchozího souboru pro
# ukládání u¾ivatelských zmìn v konfiguraci (po vypu¹tìní speciálních znakù a
# diakritiky)
#application_name = 'Pytis'

# Velikost cache pro øádky datového objektu. Velikost je celé èíslo, které
# udává poèet øádkù cache.
#cache_size = 20000

# Barva zvýraznìní aktivní buòky tabulkového formuláøe. Barva je reprezentována
# øetìzcem '#RRGGBB'.
#cell_highlight_color = '#ffa000'

# Flag urèující, zda má být spou¹tìn dohlí¾eè zmìn dat.
#dblisten = True

