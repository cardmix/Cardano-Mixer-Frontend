function runHeadScripts() {
  WebFont.load(
    {google:{families:
      ['Droid Serif:400,400italic,700,700italic',
      'Corben:regular','Fenix:regular']}});
  !function(o,c) {
    var n=c.documentElement, t=' w-mod-';
    n.className+=t+'js';
    if ('ontouchstart'in o||o.DocumentTouch && c instanceof DocumentTouch) {
      n.className+=t+'touch';
    }
  }(window,document);
};

function copyElemContent(elId) {
  var el = document.getElementById(elId);
  if (el != null && navigator && navigator.clipboard && navigator.clipboard.writeText) {
    navigator.clipboard.writeText(el.innerText);
  }
};

function saveTextFile(txt) {
  var element = document.createElement('a');
  element.setAttribute('href', 'data:application/octet-stream,' +
    encodeURIComponent(txt));
  element.setAttribute('download', 'key.txt');
  element.style.display = 'none';
  document.body.appendChild(element);
  element.click();
  document.body.removeChild(element);
};

function autofillAddr(elId) {
  var p = Promise.all([window.cardano.getUsedAddresses(), window.cardano.getUnusedAddresses()]);
  p().then(([walletUsedAddresses, walletUnusedAddresses]) =>
    {
      const addresses = walletUnusedAddresses.concat(walletUsedAddresses)
      if (addresses.length > 0) {
        setInputValue(elId, a[0]);
      };
    });
};

function setInputValue(elId, val) {
  var el = document.getElementById(elId);
  if (el != null) {
    el.value = val;
    var eChange = new Event('change');
    var eInput = new Event('input');
    el.dispatchEvent(eChange);
    el.dispatchEvent(eInput);
  };
};
