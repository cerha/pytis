# Příznak cachování specifikací při startu aplikace.
#cache_spec_onstart = True

# Port databázového serveru.
#dbport = None

# Příznak zobrazování úvodního uvítacího dialogu.
#show_splash = True

# Barva mřížky tabulkového formuláře. Barva je reprezentována řetězcem
# '#RRGGBB'.
#grid_line_color = '#6482be'

# Barva záhlaví tabulky při zapnutém filtrování. Barva je reprezentována
# řetězcem '#RRGGBB'.
#filter_color = '#82c882'

# E-mailová adresa, na kterou mají být posílána oznámení o chybě.
#bug_report_address = ''

# Cesta k souboru s logem. Může být zadán absolutně i relativně vzhledem
# k aktuálnímu adresáři.
#logo = './icons/logo.bmp'

# Shellový příkaz sendmail včetně celé cesty.
#sendmail_command = '/usr/lib/sendmail'

# Počet řádků, které se přinačítají do cache při dalších selectech z datového
# objektu.
#fetch_size = 100

# Numeric locale. Hodnota musí být string reprezentující locale pro formátování
# číselných položek.
#lc_numeric = 'C'

# Barva pozadí needitovatelného vstupního políčka. Barva je reprezentována
# řetězcem '#RRGGBB'.
#field_disabled_color = '#c0c0c0'

# Jméno tabulky, do které mají být logovány DML SQL příkazy.
#dblogtable = ''

# Počet řádků, které se přednačtou do cache při prvním selectu z datového
# objektu.
#initial_fetch_size = 100

# Příznak vyvolání debuggeru při chybě. Dojde-li k odchycení neočekávané
# výjimky a tato volba je zapnuta, je vyvolán interaktivní debugger.  Je-li
# zapnuta volba 'debug', je implicitně zapnuta i tato volba.  Užitečné pouze
# pro ladění.
#debug_on_error = False

# Kódování exportovaných dat. Hodnota musí být jedním z podporovaných kódování
# v Pythonu.
#export_encoding = 'iso8859-2'

# Adresář s obrázkovými soubory. Může být zadán absolutně i relativně vzhledem
# k aktuálnímu adresáři.
#icon_dir = './icons'

# Sekvence jmen tříd, jejichž debugovací hlášky jsou propuštěny. Debugovací
# logovací hlášky ostatních tříd jsou odfiltrovány. Je-li 'None', jsou
# propuštěny všechny hlášky (nestanoví-li jiný filtr jinak).  Užitečné pouze
# pro ladění.
#log_class_filter = ('pytis.data.DBDefaultClass',)

# Barva pozadí editovaného řádku. Barva je reprezentována řetězcem '#RRGGBB'.
#row_edit_bg_color = '#c80000'

# Subject mailu oznámení o chybě aplikace.
#bug_report_subject = 'Bug report: Unexpected exception'

# Uživatelské jméno (login) pro databázové spojení.
#dbuser = getpass.getuser()

# Barva pozadí aktivního řádku tabulkového formuláře. Pokud barva není
# nastavena, bude použita systémová barva zvýraznění.  Barva je reprezentována
# řetězcem '#RRGGBB'.
#row_focus_bg_color = None

# Pozadí políčka needitovatelného kvůli přístupovým právům. Barva je
# reprezentována řetězcem '#RRGGBB'.
#field_inaccessible_color = '#e0e4f0'

# Příznak profilování. Je-li zapnut, aplikace se spustí v profilovacím režimu a
# ukládá informace o trvání jednotlivých volání do souboru. Zapnutí této volby
# velmi výrazně zpomaluje běh aplikace.
#profile = False

# Adresář obsahující definiční soubory. Adresář může být zadán absolutně
# i relativně vzhledem k aktuálnímu adresáři.
#def_dir = './defs'

# Adresář pro dočasné pomocné soubory.
#tmp_dir = '/tmp'

# Barva textu neaktivního řádku tabulkového formuláře. Barva je reprezentována
# řetězcem '#RRGGBB'.
#row_nofocus_fg_color = '#000000'

# Příznak ladícího režimu. Je-li zapnut, aplikace může běžet s více kontrolami
# a vypisovat spoustu informací, obvykle však za cenu svého výrazného
# zpomalení.
#debug = False

# Specifikace logovací třídy. Trojice (CLASS, ARGS, KWARGS), kde CLASS je
# logovací třída a ARGS, resp. KWARGS, jsou argumenty, resp. klíčované
# argumenty, jejího konstruktoru.  Standardní dostupné třídy jsou SyslogLogger
# a StreamLogger.  Více o nich lze nalézt v jejich dokumentaci.
#log_logger = (log.StreamLogger, (sys.stderr,), {})

# Adresář pro export do CSV souborů. Hodnota udává cestu k adresáři, kde se
# budou ukládat textové CSV soubory.
#export_directory = '/tmp'

# Heslo pro přihlášení k databázi.
#dbpass = None

# Formát času. Řetězec ve tvaru vyžadovaném parametrem `format' konstruktoru
# třídy 'pytis.data.Time'.
#time_format = '%H:%M:%S'

# Adresář obsahující soubory s nápovědou. Může být zadán absolutně i relativně
# vzhledem k aktuálnímu adresáři.
#help_dir = './help'

# Seznam formulářů, které mají být otevřeny po spuštění aplikace.
#startup_forms = None

# Příznak výpisu ladících informací o paměti. Je-li zapnuta, aplikace vypisuje
# informativní hlášky garbage collectoru a jiné údaje o paměti.
#debug_memory = False

# Určuje, zda je preferováno stručné nebo jednotné formátování. Je-li tato
# volba nastavena na pravdu, jsou krátká data v logovacích hláškách
# doporučujících stručnost připojena ihned za hlášku místo vypsání na
# samostatný řádek.
#log_one_line_preferred = True

# Ztmavení barvy skupiny při seskupování řádků. Protože barva pozadí řádků není
# vždy bílá, je tato hodnota chápána jako relativní.  O kolik je zvolená barva
# tmavší než bílá, o tolik bude výsledná barva skupiny tmavší, než barva pozadí
# ostatních řádků.  Barva je reprezentována řetězcem '#RRGGBB'.
#grouping_background_downgrade = '#eceef0'

# Jméno aplikační databáze.
#dbname = 'pytis'

# Adresář obsahující dokumentaci.
#doc_dir = './docs'

# Formát data. Řetězec ve tvaru vyžadovaném parametrem `format' konstruktoru
# třídy 'pytis.data.Date'.
#date_format = '%Y-%m-%d'

# Seznam typů logovacích hlášek, které mají být odfiltrovány. V seznamu lze
# použít konstanty 'OPERATIONAL', 'ACTION', 'EVENT' a 'DEBUG'.
#log_exclude = [DEBUG]

# Prefix jména modulu, jehož debugovací hlášky jsou propuštěny. Debugovací
# logovací hlášky modulů s jiným prefixem jsou odfiltrovány.  Není-li
# definováno, jsou propuštěny všechny hlášky (nestanoví-li jiný filtr jinak).
# Užitečné pouze pro ladění.
#log_module_filter = 'pytis.data'

# Formát společně uvedeného data a času. Řetězec ve tvaru vyžadovaném
# parametrem `format' konstruktoru třídy 'pytis.data.DateTime'.
#date_time_format = '%Y-%m-%d %H:%M:%S'

# Roztahovat sloupce tabulek, aby využily celou šířku okna.
#stretch_tables = True

# Shellový příkaz pro provedení tisku, včetně argumentů. Příkaz musí být
# schopen převzít tisková data ze standardního vstupu.
#printing_command = 'lpr'

# Jméno databázového serveru.
#dbhost = 'localhost'

# Barva textu aktivního řádku tabulkového formuláře. Barva je reprezentována
# řetězcem '#RRGGBB'.
#row_focus_fg_color = '#ffffff'

# Barva textu editovaného řádku tabulkového formuláře. Barva je reprezentována
# řetězcem '#RRGGBB'.
#row_edit_fg_color = '#ffffff'

# Barva pozadí neaktivního řádku tabulkového formuláře. Barva je reprezentována
# řetězcem '#RRGGBB'.
#row_nofocus_bg_color = '#b6b6b6'

# Příznak zobrazování bublinové nápovědy.
#show_tooltips = True

# Jméno aplikace. Jméno může být libovolné, používá se např. jako titulek okna
# nebo při logování.  Od něho je také odvozeno jméno výchozího souboru pro
# ukládání uživatelských změn v konfiguraci (po vypuštění speciálních znaků a
# diakritiky)
#application_name = 'Pytis'

# Velikost cache pro řádky datového objektu. Velikost je celé číslo, které
# udává počet řádků cache.
#cache_size = 20000

# Barva zvýraznění aktivní buňky tabulkového formuláře. Barva je reprezentována
# řetězcem '#RRGGBB'.
#cell_highlight_color = '#ffa000'

# Flag určující, zda má být spouštěn dohlížeč změn dat.
#dblisten = True

