'use strict';

const fs = require('fs');

const tree = new Map();
let maxWordLen = 0;

fs.readFileSync(`${__dirname}/../../dict_en.txt`).toString().split(/\r?\n/).forEach(word => {
  maxWordLen = Math.max(word.length, maxWordLen);

  let subtree = tree;
  for (const char of word) {
    if (!subtree.has(char)) {
      subtree.set(char, new Map());
    }
    subtree = subtree.get(char);
  }
  subtree.set(true, true); // indicate and of a word
});
console.log(maxWordLen);
console.log(tree.get('w').get('o').get('o').get('d').get(true));

function getMatches(text, startIndex) {
  const matches = [];

  let subtree = tree;
  for (let i = startIndex; i < text.length; i++) {
    const char = text[i];
    if (!subtree.has(char)) { break; }

    subtree = subtree.get(char);
    if (subtree.has(true)) {
      matches.push(text.slice(startIndex, i + 1));
    }
  }

  return matches.length > 0 ? matches.reverse() : null;
}
console.log(getMatches('thousandfoldlyabc', 0));
console.log(getMatches('thousandfoldlyabc', 8));
console.log(getMatches('tthousandfoldlyabc', 1));
console.log(getMatches(':-)', 0));

function getGreedySolution(src) {
  let result = [];

  for (let i = 0; i < src.length; i++) {
    const matches = getMatches(src, i);
    if (matches) {
      result.push(matches[0]);
      i += matches[0].length;
      i--;
    } else {
      result.push(src[i]);
    }
  }

  return result.join(' ');
}
console.log(getGreedySolution('thisisatest'));
console.log(getGreedySolution('thousandfoldlyisastrangeword'));

function getAllSolution(src) {
  if (src.length < 2) {
    return src;
  }

  const solutionsBySrcLen = new Array(src.length + 1);
  solutionsBySrcLen[0] = [''];
  for (let i = 1; i < solutionsBySrcLen.length; i++) {
    solutionsBySrcLen[i] = [];
  }

  for (let i = 0; i < src.length; i++) {
    for (const match of (getMatches(src, i) || [src[i]])) {
      for (const prevSolution of solutionsBySrcLen[i]) {
        solutionsBySrcLen[i + match.length].push(`${prevSolution} ${match}`);
      }
    }
  }

  return solutionsBySrcLen.pop().sort((s1, s2) => s1.length - s2.length).map(s => s.slice(1));
}

console.log(getAllSolution('thisisatest'));
console.log(getAllSolution('thousandfoldlyisastrangeword'));
