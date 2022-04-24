const loader = new Loader();

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
  
  loader.load();
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

// Convert a hex string to a byte array
function fromHexString(hex) {
  for (var bytes = [], c = 0; c < hex.length; c += 2)
      bytes.push(parseInt(hex.substr(c, 2), 16));
  return bytes;
}

function toHexString(byteArray) {
  return Array.from(byteArray, function(byte) {
    return ('0' + (byte & 0xFF).toString(16)).slice(-2);
  }).join('')
}

function autofillAddr(elId) {
  const abc = new Loader()
  return abc.load()
    .then(() => {
      const CardanoWasm = abc.Cardano
      window.cardano.enable();
      var p = Promise.all([window.cardano.getUsedAddresses(), window.cardano.getUnusedAddresses()]);
      p.then(([walletUsedAddresses, walletUnusedAddresses]) =>
        {
          const addresses = walletUnusedAddresses.concat(walletUsedAddresses)
          if (addresses.length > 0) {
            const address = CardanoWasm.Address.from_bytes(fromHexString(addresses[0])).to_bech32()
            setInputValue(elId, address);
          };
        });
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
    el.innerText = val;
  };
};

// function runDeposit(elId, arg) {
//   namiBalanceTx(arg).
//     then((val1) => {
//       setElementText(elId, "Сonfirm the transaction in your wallet");
//       window.cardano.signTx(val1).
//         then((val2) => window.cardano.submitTx(val2).
//           then((res) => setElementText(elId, "Transaction is confirmed!"))
//         );
//     });
// };

function runDeposit(elId, elTx, arg) {
  setElementText(elId, "Сonfirm the transaction in your wallet");
  window.cardano.nami.enable().
    then((api) => {
      const abc = new Loader();
      return abc.load()
      .then(() => {
        const CardanoWasm = abc.Cardano;
        const transaction = CardanoWasm.Transaction.from_bytes(fromHexString(arg));
        const transactionWitnessSet = transaction.witness_set();
        const transactionBody = transaction.body();
        api.signTx(arg, true).
          then((res) => {
            const txVkeyWitnesses = CardanoWasm.TransactionWitnessSet.from_bytes(fromHexString(res));
            transactionWitnessSet.set_vkeys(txVkeyWitnesses.vkeys());
            const readyToSubmit = CardanoWasm.Transaction.new(transactionBody, transactionWitnessSet);
            const finalTx = toHexString(readyToSubmit.to_bytes());
            setInputValue(elTx, finalTx);
      }, (res) => { setInputValue(elTx, ""); })
    },
      (res) => { setInputValue(elTx, ""); })
    })
};

