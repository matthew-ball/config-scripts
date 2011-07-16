// ============================
// custom .conkerorrc file
// Matthew Ball (copyleft 2011)
// ============================

// ==========
/// variables
// ==========
load_paths.unshift("chrome://conkeror-contrib/content/"); // load path: allow for 'contrib' stuff
homepage = "http://www.google.com/ig"; // homepage
dowload_buffer_automatic_open_target = [OPEN_NEW_BUFFER_BACKGROUND, OPEN_NEW_WINDOW]; // open downloads in a new buffer
minibuffer_auto_complete_default = true; // auto-completion in the mini-buffer
minibuffer_read_url_select_initial = false; // T and O shouldn't keave the URL highlighted
url_completion_use_webjumps = true; // complete webjumps
url_completion_use_history = true; // should work since (bf05c87405)
url_completion_use_bookmarks = false; // bookmarks are now done through webjump
url_remoting_fn = load_url_in_new_buffer; // open external links in a new buffer
hints_display_url_panel = true; // display properties of the current selected node during the hints interaction
can_kill_last_buffer = false;
view_source_use_external_editor = true; // view page source in editor
isearch_keep_selection = true; // keep found item selected after search-mode ends

// ===========
/// MIME types
// ===========
content_handlers.set("application/pdf", content_handler_save); // automatically handle some mime types internally

external_content_handlers.set("application/pdf", "evince");
external_content_handlers.set("application/x-dvi", "evince");

// ==========
/// mode-line
// ==========
require("mode-line.js");
// require("mode-line-buttons.js");

// mode_line_add_buttons(standard_mode_line_buttons, true);
// add_hook("mode_line_hook", mode_line_adder(buffer_icon_widget), true)
add_hook("mode_line_hook", mode_line_adder(loading_count_widget), true);
add_hook("mode_line_hook", mode_line_adder(buffer_count_widget), true);
// add_hook("mode_line_hook", mode_line_adder(current_buffer_name_widget));
remove_hook("mode_line_hook", mode_line_adder(clock_widget));

// =============
/// tab-bar mode
// =============
// require("new-tabs.js"); // show tabs
require("clicks-in-new-buffer.js"); // open buffers (tabs) in the background

clicks_in_new_buffer_target = OPEN_NEW_BUFFER_BACKGROUND; 
clicks_in_new_buffer_button = 1; //  middle-click opens links in new buffers

// ========================
/// webjumps and smartlinks
// ========================
define_webjump("bookmark", function(term) {return term;},
               $completer = history_completer($use_history = false,
                                              $use_bookmarks = true,
                                              $match_required = true),
               $description = "Visit a Conkeror bookmark.");

// webjumps
define_webjump("fb", "http://www.facebook.com", $description = "Facebook"); // account (not logged in)
define_webjump("g+", "http://plus.google.com", $description = "Google+"); // account (logged in)
define_webjump("gi", "http://google.com/ig", $description = "iGoogle"); // account (logged in)
define_webjump("reddit", "http://www.reddit.com", $description = "Reddit"); // account (logged in)
define_webjump("uf", "http://ubuntuforums.org", $description = "Ubuntu Forums"); // account (logged in)
define_webjump("afl", "http://www.afl.com.au", $description = "Australian Football League");
define_webjump("modem", "http://gateway.2wire.net", $description = "Telstra Modem Information");

define_webjump("conk", "http://conkeror.org", $description = "Conkeror Wiki");
define_webjump("ew", "http://emacswiki.org", $description = "Emacs Wiki");
define_webjump("stumpwmwiki", "http://stumpwm.antidesktop.net/cgi-bin/wiki.pl", $description = "StumpWM Wiki");

define_webjump("pp", "http://philpapers.org", $description = "Philosophy Papers"); // account (not logged in)
define_webjump("stanford", "http://plato.stanford.edu", $description = "Stanford Encyclopedia of Philosophy");

// define_webjump("jstor", "http://www.jstor.org");
define_webjump("jstor", "http://www.jstor.org.virtual.anu.edu.au", $description = "Journal Storage");
define_webjump("anu", "http://www.anu.edu.au", $description = "Australian National University");
define_webjump("library", "http://anulib.anu.edu.au", $description = "ANU Library");
define_webjump("wattle", "https://wattle.anu.edu.au", $description = "ANU Wattle");
define_webjump("webmail", "https://anumail.anu.edu.au", $description = "ANU Webmail");
define_webjump("isis", "https://esapps.anu.edu.au/sscsprod/psp/sscsprod", $description = "ANU ISIS and HORUS");
define_webjump("aarnet", "http://www.aarnet.edu.au", $description = "Australia's Academic and Reseach Network");

// smartlinks
define_webjump("youtube", "http://www.youtube.com/results?search_query=%s&search=Search", $alternative="http://www.youtube.com");
define_webjump("emacswiki", "http://www.google.com/cse?cx=004774160799092323420%3A6-ff2s0o6yi&q=%s&sa=Search&siteurl=emacswiki.org%2F", $alternative="http://www.emacswiki.org");
define_webjump("org-mode","https://www.google.com/cse?cx=002987994228320350715%3Az4glpcrritm&q=%s&sa=Search&siteurl=orgmode.org%2Fworg", $alternative="http://orgmode.org");

define_webjump("hoogle", "http://haskell.org/hoogle/?hoogle=%s", $alternative = "http://haskell.org/hoogle/");
define_webjump("commandlinefu", 
	       function(term) {
		 return 'http://www.commandlinefu.com/commands/matching/' +
		   term.replace(/[^a-zA-Z0-9_\-]/g, '')
		   .replace(/[\s\-]+/g, '-') + '/' + btoa(term);
	       }, $argument = 'optional', $alternative = "http://www.commandlinefu.com/");

// google specialised searching
define_webjump("scholar", "http://scholar.google.com/scholar?q=%s", $alternative = "http://scholar.google.com");
define_webjump("books", "http://www.google.com/search?q=%s&tbm=bks", $alternative = "http://books.google.com");

// ubuntu (launchpad) package search
define_webjump("ubuntupkg", "http://packages.ubuntu.com/%s");
define_webjump("ubuntufile", "http://packages.ubuntu.com/search?searchon=contents&keywords=%s&mode=path&arch=any");
define_webjump("ubuntubugs", "http://bugs.launchpad.net/ubuntu/+source/%s");
define_webjump("launchpad", "https://launchpad.net/+search?field.text=%s");

define_webjump("github", "http://github.com/search?q=%s", $alternative="http://github.com"); // github search
define_webjump("gitorious", "http://gitorious.org/search?q=%s", $alternative="http://gitorious.org"); // gitorious search

read_url_handler_list = [read_url_make_default_webjump_handler("google")]; // default webjump

// ===========
/// quickjumps
// ===========
interactive("open-gmail", "Open gmail inbox.", "follow", $browser_object = "http://gmail.com/"); // open gmail (an alias of the follow command)

interactive("open-school-all","Open school related web-sites.",
	    function(I){
	      load_url_in_new_buffer("http://wattle.anu.edu.au",I.window); // wattle
	      load_url_in_new_buffer("http://anumail.anu.edu.au",I.window); // webmail
	    });

// ==================
/// emacs integration
// ==================
editor_shell_command = "emacsclient -c"; // edit form text with emacs

// TODO: fix this up
function org_capture (url, title, selection, window) { // org-protocol stuff
  // var cmd_str = 'emacsclient \"org-protocol:/capture:/w/'+url+'/'+title+'/'+selection+'\"';
  var cmd_str = 'emacsclient \"org-protocol:/capture:/k/'+url+'/'+title+'/"';
  if (window != null) {
    window.minibuffer.message('Issuing: ' + cmd_str);
  }
  shell_command_blind(cmd_str);
}

interactive("org-capture", "Clip url, title, and selection to capture via org-protocol.",
	    function (I) {
              org_capture(encodeURIComponent(I.buffer.display_uri_string),
			  encodeURIComponent(I.buffer.document.title),
			  encodeURIComponent(I.buffer.top_frame.getSelection()),
			  I.window);
	    });

// ===============
/// user functions
// ===============
interactive("copy-url", "Copy the current buffer's URL to the clipboard.",
	    function(I) {
	      var text = I.window.buffers.current.document.location.href;
	      writeToClipboard(text);
	      I.window.minibuffer.message("copied: " + text);
	    });

interactive("reload-config", "Reload ~/.conkerorrc file.",
	    function(I) {
	      load_rc();
	      I.window.minibuffer.message("Config file reloaded.");
	    });

function url_completion_toggle (I) {
  if (url_completion_use_bookmarks) {
    url_completion_use_bookmarks = false;
    url_completion_use_history = true;
  } else {
    url_completion_use_bookmarks = true;
    url_completion_use_history = false;
  }
}

interactive("url-completion-toggle", "toggle between bookmark and history completion", url_completion_toggle);

// =============
/// key bindings
// =============
key_bindings_ignore_capslock = true;

define_key(default_global_keymap, "C-c u", "copy-url"); // copy url with C-c u
define_key(default_global_keymap, "C-c r", "reload-config"); // reload config with C-c r
define_key(content_buffer_normal_keymap, "C-c c", "org-capture"); // capture with C-c c
define_key(content_buffer_normal_keymap, "C-c t", "url-completion-toggle"); // url completion with C-c t
define_key(content_buffer_normal_keymap, "f1", "open-school-all"); // open school urls with f1
define_key(content_buffer_normal_keymap, "f2", "open-gmail"); // open gmail inbox with f2

// ==========
/// xkcd mode
// ==========
xkcd_add_title = true;

// ===============
/// wikipedia mode
// ===============
require("page-modes/wikipedia.js")
wikipedia_enable_didyoumean = true; // automatically follow "did you mean" links on wikipedia search pages

// ========================
/// auto-hide the mode-line
// ========================
// var minibuffer_autohide_timer = null;
// var minibuffer_autohide_message_timeout = 3000; //milliseconds to show messages
// var minibuffer_mutually_exclusive_with_mode_line = true;

// function hide_minibuffer (window) {
//   window.minibuffer.element.collapsed = true;
//   if (minibuffer_mutually_exclusive_with_mode_line && window.mode_line)
//     window.mode_line.container.collapsed = false;
// }

// function show_minibuffer (window) {
//   window.minibuffer.element.collapsed = false;
//   if (minibuffer_mutually_exclusive_with_mode_line && window.mode_line)
//     window.mode_line.container.collapsed = true;
// }

// add_hook("window_initialize_hook", hide_minibuffer);
// // for_each_window(hide_minibuffer); // initialize existing windows

// var old_minibuffer_restore_state = (old_minibuffer_restore_state || minibuffer.prototype._restore_state);

// minibuffer.prototype._restore_state = function () {
//   if (minibuffer_autohide_timer) {
//     timer_cancel(minibuffer_autohide_timer);
//     minibuffer_autohide_timer = null;
//   }
//   if (this.current_state)
//     show_minibuffer(this.window);
//   else
//     hide_minibuffer(this.window);
//   old_minibuffer_restore_state.call(this);
// };

// var old_minibuffer_show = (old_minibuffer_show || minibuffer.prototype.show);

// minibuffer.prototype.show = function (str, force) {
//   var w = this.window;
//   show_minibuffer(w);
//   old_minibuffer_show.call(this, str, force);
//   if (minibuffer_autohide_timer)
//     timer_cancel(minibuffer_autohide_timer);
//   minibuffer_autohide_timer = call_after_timeout(
//     function () { hide_minibuffer(w); },
//     minibuffer_autohide_message_timeout);
// };

// var old_minibuffer_clear = (old_minibuffer_clear || minibuffer.prototype.clear);

// minibuffer.prototype.clear = function () {
//   if (minibuffer_autohide_timer) {
//     timer_cancel(minibuffer_autohide_timer);
//     minibuffer_autohide_timer = null;
//   }
//   if (! this.current_state)
//     hide_minibuffer(this.window);
//   old_minibuffer_clear.call(this);
// };

// =======
/// google
// =======
// register_user_stylesheet(
//   "data:text/css,"+
//   escape(
//     "@-moz-document url-prefix(http://www.google.com/search?)"+
//     "{#leftnav {display: none !important;}"+
//     "#center_col {margin-left: 0px !important;}}"));

// ========
/// session
// ========
require("session.js");
// session_auto_save_file = "./session"
session_auto_save_auto_load = true; // autmoatically load saved session on startup
// session_auto_save_auto_load = "prompt";

// =======
/// daemon
// =======
// NOTE: apparently the session module does not work correctly with daemon
// require('daemon');
// daemon_mode(1);
