'use strict';

const fs = require('fs');

const dictTree = new Map();

for (const word of fs.readFileSync(`${__dirname}/../../dict_en.txt`).toString().split(/\r?\n/)) {
  let subtree = dictTree;
  for (const char of word) {
    if (!subtree.has(char)) {
      subtree.set(char, new Map());
    }
    subtree = subtree.get(char);
  }
  subtree.set(true, true); // indicate the end of a word
}
console.log(dictTree.get('w').get('o').get('o').get('d').get(true));

function getDictMatches(text, startIndex) {
  const matches = [];

  let subtree = dictTree;
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
console.log(getDictMatches('thousandfoldlyabc', 0));
console.log(getDictMatches('thousandfoldlyabc', 8));
console.log(getDictMatches(':-)', 0));

function getGreedySolution(text) {
  let result = [];

  for (let i = 0; i < text.length; i++) {
    const matches = getDictMatches(text, i);
    if (matches) {
      const bestMatch = matches[matches.length - 1];
      result.push(bestMatch);
      i += bestMatch.length;
      i--;
    } else {
      result.push(text[i]);
    }
  }

  return result.join(' ');
}
console.log(getGreedySolution('thisisatest'));
console.log(getGreedySolution('thousandfoldlyisastrangeword'));

function getSolutionsTree(text) {
  if (text.length < 2) {
    return new Map([[text, new Map()]]);
  }

  const solutionsBySrcLen = new Map();
  for (let i = 0; i <= text.length; i++) {
    solutionsBySrcLen.set(i, new Map());
  }

  for (let i = 0; i < text.length; i++) {
    for (const match of (getDictMatches(text, i) || [text[i]])) {
      solutionsBySrcLen.get(i + match.length).set(match, solutionsBySrcLen.get(i));
    }
  }

  return solutionsBySrcLen.get(text.length);
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
