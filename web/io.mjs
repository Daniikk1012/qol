const outputElement = document.getElementById('output');

export function output(s) {
  outputElement.innerText += s;
}

export function input() {
  const line = prompt('Enter a line of input:');
  outputElement.innerText += line + '\n';
  return line;
}
