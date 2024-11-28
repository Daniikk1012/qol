const outputElement = document.getElementById('output');

function escapeHtml(unsafe) {
  return unsafe
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&#039;')
    .replace(/ /g, '&nbsp;')
    .replace(/\n/g, '<br>');
}

export function output(s) {
  outputElement.innerHTML += escapeHtml(s);
}

export function input() {
  const line = prompt('Enter a line of input:');
  outputElement.innerHTML += escapeHtml(line + '\n');
  return line;
}
