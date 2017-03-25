'use strict';

const fs = require('fs');

const tree = new Map();
let maxWordLen = 0;

for (const word of fs.readFileSync(`${__dirname}/../../dict_en.txt`).toString().split(/\r?\n/)) {
  maxWordLen = Math.max(word.length, maxWordLen);

  let subtree = tree;
  for (const char of word) {
    if (!subtree.has(char)) {
      subtree.set(char, new Map());
    }
    subtree = subtree.get(char);
  }
  subtree.set(true, true); // indicate and of a word
}
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

  return matches.length > 0 ? matches : null;
}
console.log(getMatches('thousandfoldlyabc', 0));
console.log(getMatches('thousandfoldlyabc', 8));
console.log(getMatches(':-)', 0));

function getGreedySolution(src) {
  let result = [];

  for (let i = 0; i < src.length; i++) {
    const matches = getMatches(src, i);
    if (matches) {
      const bestMatch = matches[matches.length - 1];
      result.push(bestMatch);
      i += bestMatch.length;
      i--;
    } else {
      result.push(src[i]);
    }
  }

  return result.join(' ');
}
console.log(getGreedySolution('thisisatest'));
console.log(getGreedySolution('thousandfoldlyisastrangeword'));

function getSolutionsTree(src) {
  if (src.length < 2) {
    return new Map([[src, new Map()]]);
  }

  const solutionsBySrcLen = new Map();
  for (let i = 0; i <= src.length; i++) {
    solutionsBySrcLen.set(i, new Map());
  }

  for (let i = 0; i < src.length; i++) {
    for (const match of (getMatches(src, i) || [src[i]])) {
      solutionsBySrcLen.get(i + match.length).set(match, solutionsBySrcLen.get(i));
    }
  }

  return solutionsBySrcLen.get(src.length);
}

function unwindSolutionsTree(tree) {
  const unwinded = [];

  while (tree.size > 0) {
    for (let [text, subtree] of tree) {
      tree.delete(text);

      if (subtree.size === 0) {
        unwinded.push(text)
      } else {
        for (let [subtext, subsubtree] of subtree) {
          tree.set(`${subtext} ${text}`, subsubtree);
        }
      }
    }
  }

  return unwinded;
}
console.log(unwindSolutionsTree(getSolutionsTree('t')));
console.log(unwindSolutionsTree(getSolutionsTree('thisisatest')));

function getBestSolutionFromTree(tree) {
  // TODO
}
