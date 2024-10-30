import init, {wasm_run} from './pkg/qol.js';

const codeArea = document.getElementById('code');
const outputElement = document.getElementById('output');

document.getElementById('run').addEventListener('click', () => {
  outputElement.innerText = '';
  init().then(() => wasm_run(codeArea.value));
});
