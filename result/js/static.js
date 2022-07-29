//////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// WebPage functions ////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

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

function setElementText(elId, val) {
  var el = document.getElementById(elId);
  if (el != null) {
    el.innerTextvalue = val;
  };
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

//////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////// App functions //////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

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

function runDeposit(elId, elTx, dp) {
  setElementText(elId, "Сonfirm the transaction in your wallet");
  window.cardano.nami.enable().
    then((api) => {
      const abc = new Loader();
      return abc.load()
      .then(() => {
        const CardanoWasm = abc.Cardano;

        // instantiate the tx builder with the Cardano protocol parameters
        const linearFee = CardanoWasm.LinearFee.new(CardanoWasm.BigNum.from_str('44'), CardanoWasm.BigNum.from_str('155381'));
        const txBuilderCfg = CardanoWasm.TransactionBuilderConfigBuilder.new()
          .fee_algo(linearFee)
          .pool_deposit(CardanoWasm.BigNum.from_str('500000000'))
          .key_deposit(CardanoWasm.BigNum.from_str('2000000'))
          .max_value_size(5000)
          .max_tx_size(16384)
          .coins_per_utxo_word(CardanoWasm.BigNum.from_str('34482'))
          .build();
        const txBuilder = CardanoWasm.TransactionBuilder.new(txBuilderCfg);

        // creating utxo value
        const valueToPay = CardanoWasm.Value.new(CardanoWasm.BigNum.from_str(dp.dpADAValue));
        const ma = CardanoWasm.MultiAsset.new();
        for (i=0; i < dp.dpNonADAValue.length; i++)
        {
          const a = CardanoWasm.Assets.new();
          a.insert(CardanoWasm.AssetName.new(fromHexString(dp.dpNonADAValue[i][1])), CardanoWasm.BigNum.from_str(dp.dpNonADAValue[i][2]));
          ma.insert(CardanoWasm.ScriptHash.from_bytes(fromHexString(dp.dpNonADAValue[i][0])), a);
        }
        valueToPay.set_multiasset(ma);

        // add inputs from the user's wallet
        valueToSpend = valueToPay.checked_add(CardanoWasm.Value.new(CardanoWasm.BigNum.from_str('10000000'))); // adding fee estimate
        console.log(valueToSpend.coin().to_str());
        api.getUtxos(toHexString(valueToSpend.to_bytes()), undefined).
          then((res) => {
            for (i=0; i < res.length; i++)
            {
              const utxo = CardanoWasm.TransactionUnspentOutput.from_bytes(fromHexString(res[i]));
              const utxoInput = utxo.input();
              const utxoOut = utxo.output();
              txBuilder.add_input(
                utxoOut.address(),
                utxoInput,
                utxoOut.amount()
              );      
            };
            
            // creating utxo datum
            const datum = CardanoWasm.PlutusData.new_integer(CardanoWasm.BigInt.from_str(dp.dpKey)); // here goes the user-generated key

            // add output to the tx
            const scriptAddress = CardanoWasm.Address.from_bech32(dp.dpAddress);
            scriptOutput = CardanoWasm.TransactionOutput.new(scriptAddress, valueToPay);
            scriptOutput.set_data_hash(CardanoWasm.hash_plutus_data(datum));
            txBuilder.add_output(scriptOutput);
            
            // calculate the min fee required and send any change to an address
            api.getChangeAddress().
            then((res) => {
              const changeAddress = CardanoWasm.Address.from_bytes(fromHexString(res));
              txBuilder.add_change_if_needed(changeAddress);

              // once the transaction is ready, we build it to get the tx body without witnesses
              const txBody = txBuilder.build();
              const txHash = CardanoWasm.hash_transaction(txBody);
              const witnesses = CardanoWasm.TransactionWitnessSet.new();

              // create the finalized transaction with witnesses
              const partialTx = CardanoWasm.Transaction.new(
                txBody,
                witnesses,
                undefined, // transaction metadata
              );
              const partialTx_hex = toHexString(partialTx.to_bytes());

              setInputValue(elTx, "");
              api.signTx(partialTx_hex, true).
                then((res) => {
                  const txVkeyWitnesses = CardanoWasm.TransactionWitnessSet.from_bytes(fromHexString(res));
                  const readyToSubmit = CardanoWasm.Transaction.new(txBody, txVkeyWitnesses);
                  const finalTx = toHexString(readyToSubmit.to_bytes());
                  api.submitTx(finalTx).
                    then((res) => {
                      setInputValue(elTx, res);
                      console.log(res);
                    }, (res) => { console.log(res); });
              }, (res) => { console.log(res); });
            }, (res) => { console.log(res) });
        }, (res) => { console.log(res) });
      }, (res) => { console.log(res); });
    }, (res) => { console.log(res); });
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