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
  const abc = new Loader()
  return abc.load()
    .then(() => {
      const CardanoWasm = abc.Cardano
      if ((typeof window.cardano === 'undefined') || (typeof window.cardano.nami === 'undefined')) {
      }
      else {
      window.cardano.nami.enable();
      var p = Promise.all([window.cardano.getUsedAddresses(), window.cardano.getUnusedAddresses()]);
      p.then(([walletUsedAddresses, walletUnusedAddresses]) =>
        {
          const addresses = walletUnusedAddresses.concat(walletUsedAddresses)
          if (addresses.length > 0) {
            const address = CardanoWasm.Address.from_bytes(fromHexString(addresses[0])).to_bech32()
            setInputValue(elId, address);
          };
        });}
      })
};

async function fillProof(elId, inputs) {
  console.log(inputs);
  const { proof, publicSignals } =
  await snarkjs.groth16.fullProve(inputs, "circuit-mixer.wasm", "circuit_final.zkey");

  setInputValue(elId, JSON.stringify(proof, null, 1));

  var el = document.getElementById(elId);
  console.log(el.value);
  console.log(publicSignals);
  // setInputValue(elId, proof);
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

function setElementText(elId, val) {
  var el = document.getElementById(elId);
  if (el != null) {
    el.innerTextvalue = val;
  };
};

function runDeposit(elId, arg) {
  namiBalanceTx(arg).
    then((val1) => {
      setElementText(elId, "Сonfirm the transaction in your wallet");
      window.cardano.signTx(val1).
        then((val2) => window.cardano.submitTx(val2).
          then((res) => setElementText(elId, "Transaction is confirmed!"))
        );
    });
};
