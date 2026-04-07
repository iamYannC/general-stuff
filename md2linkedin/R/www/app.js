function getTA() { return document.getElementById('md_input'); }


// Helper to insert text preserving native Ctrl+Z whenever possible
function safeInsertText(ta, start, end, newText) {
  ta.setSelectionRange(start, end);
  ta.focus();
  var success = document.execCommand('insertText', false, newText);
  if (!success) {
    var txt = ta.value;
    ta.value = txt.substring(0, start) + newText + txt.substring(end);
  }
}

function notifyShiny() {
  var ta = getTA();
  Shiny.setInputValue('md_input', ta.value, {priority:'event'});
}

// Wrap selected text with prefix/suffix
function wrapSelection(prefix, suffix) {
  var ta = getTA(), s = ta.selectionStart, e = ta.selectionEnd;
  var txt = ta.value, sel = txt.substring(s, e);
  var repl = prefix + (sel || 'text') + suffix;
  safeInsertText(ta, s, e, repl);
  ta.selectionStart = s + prefix.length;
  ta.selectionEnd = s + repl.length - suffix.length;
  notifyShiny();
}

// Apply combining character to selection
function applyCombining(cp) {
  var ta = getTA(), s = ta.selectionStart, e = ta.selectionEnd;
  if (s === e) return;
  var txt = ta.value, sel = txt.substring(s, e);
  var out = '';
  for (var i = 0; i < sel.length; i++) {
    out += sel[i] + String.fromCodePoint(cp);
  }
  safeInsertText(ta, s, e, out);
  ta.selectionStart = ta.selectionEnd = s + out.length;
  notifyShiny();
}

function insertAtLine(prefix) {
  var ta = getTA(), s = ta.selectionStart;
  var txt = ta.value;
  var lineStart = txt.lastIndexOf('\n', s - 1) + 1;
  safeInsertText(ta, lineStart, lineStart, prefix);
  ta.selectionStart = ta.selectionEnd = s + prefix.length;
  notifyShiny();
}

// Link modal
function openLinkModal() {
  var ta = getTA();
  var sel = ta.value.substring(ta.selectionStart, ta.selectionEnd);
  document.getElementById('link_text').value = sel || '';
  document.getElementById('link_url').value = 'https://';
  document.getElementById('link_modal').classList.add('active');
  setTimeout(function(){ document.getElementById('link_url').focus(); }, 100);
}
function closeLinkModal() {
  document.getElementById('link_modal').classList.remove('active');
}
function confirmLink() {
  var text = document.getElementById('link_text').value || 'link';
  var url  = document.getElementById('link_url').value || '';
  closeLinkModal();
  var ta = getTA(), s = ta.selectionStart, e = ta.selectionEnd;
  var md = '[' + text + '](' + url + ')';
  safeInsertText(ta, s, e, md);
  ta.selectionStart = ta.selectionEnd = s + md.length;
  notifyShiny();
}

// Copy to clipboard
function copyOutput() {
  var el = document.getElementById('preview_out');
  if (!el) return;
  navigator.clipboard.writeText(el.textContent).then(function(){
    var btn = document.getElementById('copy_btn');
    btn.classList.add('success');
    btn.innerHTML = '<i class="bi bi-check-lg"></i><span>Copied!</span>';
    setTimeout(function(){
      btn.classList.remove('success');
      btn.innerHTML = '<i class="bi bi-clipboard"></i><span>Copy</span>';
    }, 1800);
  });
}

// Character counts
function updateCounts() {
  var ta = getTA();
  var len = ta.value.length;
  document.getElementById('input_count').textContent = len + ' characters';
}

$(document).on('shiny:connected', function(){
  var ta = getTA();
  if(ta) { ta.addEventListener('input', updateCounts); }
});

// Keyboard shortcuts
document.addEventListener('keydown', function(e) {
  if (!e.ctrlKey && !e.metaKey) return;
  if (e.key === 'b') { e.preventDefault(); wrapSelection('**','**'); }
  if (e.key === 'i') { e.preventDefault(); wrapSelection('*','*'); }
  if (e.key === 'u') { e.preventDefault(); applyCombining(0x0332); }
  if (e.key === 'k') { e.preventDefault(); openLinkModal(); }
  if (e.key === '1') { e.preventDefault(); insertAtLine('# '); }
  if (e.key === '2') { e.preventDefault(); insertAtLine('## '); }
  if (e.key === '3') { e.preventDefault(); insertAtLine('### '); }
  if (e.key === '4') { e.preventDefault(); insertAtLine('#### '); }
  if (e.key === '5') { e.preventDefault(); insertAtLine('##### '); }
  if (e.key === '6') { e.preventDefault(); insertAtLine('###### '); }
});

// Enter key in link modal
document.addEventListener('keydown', function(e) {
  if (e.key === 'Enter' && document.getElementById('link_modal').classList.contains('active')) {
    e.preventDefault(); confirmLink();
  }
  if (e.key === 'Escape') closeLinkModal();
});
// Theme toggle
function toggleTheme() {
  var html = document.documentElement;
  var current = html.getAttribute('data-theme');
  var next = current === 'light' ? 'dark' : 'light';
  html.setAttribute('data-theme', next);
  localStorage.setItem('md2li-theme', next);
  updateThemeIcon(next);
}
function updateThemeIcon(theme) {
  var icon = document.getElementById('theme_icon');
  if (!icon) return;
  icon.className = theme === 'light' ? 'bi bi-moon-stars-fill' : 'bi bi-sun-fill';
}
// Load saved theme on start
(function() {
  var saved = localStorage.getItem('md2li-theme');
  var systemDark = window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches;
  var theme = saved ? saved : (systemDark ? 'dark' : 'light');
  if (theme !== 'dark') {
    document.documentElement.setAttribute('data-theme', theme);
  }
  document.addEventListener('DOMContentLoaded', function() { updateThemeIcon(theme); });
})();
// Emoji picker
function toggleEmojiPicker() {
  var wrap = document.getElementById('emoji_picker_wrap');
  if (!wrap) return;
  var visible = wrap.style.display !== 'none';
  wrap.style.display = visible ? 'none' : 'block';
}

// Wire up emoji selection once DOM ready
$(document).on('shiny:connected', function() {
  var picker = document.getElementById('emoji_picker');
  if (picker) {
    picker.addEventListener('emoji-click', function(e) {
      var ta = getTA();
      var s = ta.selectionStart, end = ta.selectionEnd;
      var emoji = e.detail.unicode;
      safeInsertText(ta, s, end, emoji);
      ta.selectionStart = ta.selectionEnd = s + emoji.length;
      notifyShiny();
      updateCounts();
      document.getElementById('emoji_picker_wrap').style.display = 'none';
    });
  }
});

// Close emoji picker on outside click
document.addEventListener('click', function(e) {
  var wrap = document.getElementById('emoji_picker_wrap');
  var btn = document.getElementById('emoji_btn');
  if (!wrap || wrap.style.display === 'none') return;
  if (!wrap.contains(e.target) && !btn.contains(e.target)) {
    wrap.style.display = 'none';
  }
});

Shiny.addCustomMessageHandler('replaceSelection', function(content) {
  var ta = document.getElementById('md_input');
  var s = ta.selectionStart, end = ta.selectionEnd;
  safeInsertText(ta, s, end, content);
  ta.selectionStart = ta.selectionEnd = s + content.length;
  Shiny.setInputValue('md_input', ta.value, {priority:'event'});
  updateCounts();
});
function applyFontStyle() {
  var ta = document.getElementById('md_input');
  var s = ta.selectionStart, e = ta.selectionEnd;
  var style = document.getElementById('font_style_select').value;
  if(s === e) return; // No selection
  var sel = ta.value.substring(s, e);
  Shiny.setInputValue('apply_style_req', {text: sel, style: style}, {priority:'event'});
}
