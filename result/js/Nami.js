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

function namiIsEnabled(elId) {
  if ((typeof window.cardano === 'undefined') || (typeof window.cardano.nami === 'undefined')) {
    setInputValue(elId, false);
  }
  else{
  window.cardano.nami.isEnabled().then(
    (val) => { setInputValue(elId, val); },
    (val) => { setInputValue(elId, false); }
  );}
};

function namiEnable(elId) {
  if ((typeof window.cardano === 'undefined') || (typeof window.cardano.nami === 'undefined')) {
    setInputValue(elId, false);
  }
  else{
  window.cardano.nami.enable().then(
    (val) => { setInputValue(elId, true); },
    (val) => { setInputValue(elId, false); }
  );}
};

function namiGetNetworkId(elId) {
  window.cardano.getNetworkId().then(
    (val) => { setInputValue(elId, val); },
    (val) => { setInputValue(elId, ""); }
  );
};

function namiGetBalance(elId) {
  window.cardano.getBalance().then(
    (val) => { setInputValue(elId, val); },
    (val) => { setInputValue(elId, ""); }
  );
};

function namiGetCollateral(elId) {
  window.cardano.getCollateral().then(
    (val) => { setInputValue(elId, val); },
    (val) => { setInputValue(elId, ""); }
  );
};

function namiGetUtxos(elId, amount) {
  window.cardano.getUtxos(amount).then(
    (val) => { setInputValue(elId, val); },
    (val) => { setInputValue(elId, ""); }
  );
};

function namiGetUsedAddresses(elId) {
  window.cardano.getUsedAddresses().then(
    (val) => { setInputValue(elId, val); },
    (val) => { setInputValue(elId, ""); }
  );
};

function namiGetUnusedAddresses(elId) {
  window.cardano.getUnusedAddresses().then(
    (val) => { setInputValue(elId, val); },
    (val) => { setInputValue(elId, ""); }
  );
};

function namiGetChangeAddress(elId) {
  window.cardano.getChangeAddress().then(
    (val) => { setInputValue(elId, val); },
    (val) => { setInputValue(elId, ""); }
  );
};

function namiGetRewardAddresses(elId) {
  window.cardano.getRewardAddresses().then(
    (val) => { setInputValue(elId, val); },
    (val) => { setInputValue(elId, ""); }
  );
};

function namiGetRewardAddresses(elId) {
  window.cardano.getRewardAddresses().then(
    (val) => { setInputValue(elId, val); },
    (val) => { setInputValue(elId, ""); }
  );
};

function namiSignData(elId, addr, payload) {
  window.cardano.signData(addr, payload).then(
    (val) => { setInputValue(elId, val); },
    (val) => { setInputValue(elId, ""); }
  );
};

function namiSignTx(elId, tx, partialSign = false) {
  window.cardano.signTx(tx, partialSign).then(
    (val) => { setInputValue(elId, val); },
    (val) => { setInputValue(elId, ""); }
  );
};

function namiSubmitTx(elId, tx) {
  window.cardano.submitTx(tx).then(
    (val) => { setInputValue(elId, val); },
    (val) => { setInputValue(elId, ""); }
  );
};

function namiGetWalletId(elId) {
  var p = Promise.all([window.cardano.getUsedAddresses(), window.cardano.getUnusedAddresses()])
    .then(([walletUsedAddresses, walletUnusedAddresses]) => {
      const addresses = walletUnusedAddresses.concat(walletUsedAddresses)
      var res = require('blake2b')(20)
        .update(Buffer.from(addresses.map(a => a.to_bech32).join('')))
        .digest('hex');
  });
  p().then(
    (val) => { setInputValue(elId, val); },
    (val) => { setInputValue(elId, ""); }
  );
};

function namiBalanceTx(txCbor) {
  return Loader.load()
    .then(() => {
      const CardanoWasm = Loader.Cardano
      return Promise.all([
        window.cardano.getChangeAddress(),
        window.cardano.getUtxos(),
        fetchProtocolParameters()
      ])
      .then(promises => {
        const changeAddrCbor = promises[0]
        const changeAddrBech32 = CardanoWasm.Address.from_bytes(fromHexString(changeAddrCbor)).to_bech32()
        const utxosCbor = promises[1]
        const utxos = utxosCbor.map(cbor => CardanoWasm.TransactionUnspentOutput.from_bytes(fromHexString(cbor)))
        const pp = promises[2]
        const tx = CardanoWasm.Transaction.from_bytes(fromHexString(txCbor))
        return buildTx({ 'paymentAddr': changeAddrBech32 }, utxos, tx.body().outputs(), pp)
      })
      .then(btx => toHexString(btx.to_bytes()))
    })
};
