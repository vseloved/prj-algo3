'use strict';

const fs = require('fs');

const dictTree = new Map();
function callAndPrint(lambda) {
  console.log(`${lambda}:`.slice(6), lambda());
}

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
callAndPrint(() => dictTree.get('w').get('o').get('o').get('d').get(true));

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
callAndPrint(() => getDictMatches('thousandfoldlyabc', 0));
callAndPrint(() => getDictMatches('thousandfoldlyabc', 8));
callAndPrint(() => getDictMatches(':-)', 0));

function restoreSpacesGreedily(text) {
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
callAndPrint(() => restoreSpacesGreedily('thisisatest'));
callAndPrint(() => restoreSpacesGreedily('thousandfoldlyisastrangeword'));

function getWordsDistance(first, second) {
  // TODO improve
  return 1;
}

function getWordsTree(text) {
  const wordsBySrcLen = new Map();
  for (let i = 0; i <= text.length; i++) {
    const words = new Map();
    words.closestWord = '';
    words.closestDistance = i === 0 ? 0 : Infinity;

    wordsBySrcLen.set(i, words);
  }

  for (let i = 0; i < text.length; i++) {
    for (const match of (getDictMatches(text, i) || [text[i]])) {
      const prevWords = wordsBySrcLen.get(i);
      const prevWordsAndMatch = wordsBySrcLen.get(i + match.length);
      prevWordsAndMatch.set(match, prevWords);

      const distance = prevWords.closestDistance + getWordsDistance(prevWords.closestWord, match);
      if (distance < prevWordsAndMatch.closestDistance) {
        prevWordsAndMatch.closestWord = match;
        prevWordsAndMatch.closestDistance = distance;
      }
    }
  }

  return wordsBySrcLen.get(text.length);
}

function unwindWordsTree(tree) {
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
callAndPrint(() => unwindWordsTree(getWordsTree('t')));
callAndPrint(() => unwindWordsTree(getWordsTree('thisisatest')));

function getOptimalFromWordsTree(tree) {
  const text = [];

  let subtree = tree;
  while (subtree) {
    text.unshift(subtree.closestWord);
    subtree = subtree.get(subtree.closestWord);
  }

  return text.join(' ').slice(1);
}
callAndPrint(() => getOptimalFromWordsTree(getWordsTree('t')));
callAndPrint(() => getOptimalFromWordsTree(getWordsTree('thisisatest')));
callAndPrint(() => getOptimalFromWordsTree(getWordsTree('thousandfoldlyisastrangeword')));
callAndPrint(() => getOptimalFromWordsTree(getWordsTree('111.aaa.bbb.3333@comisateste-mail.')));
callAndPrint(() => getOptimalFromWordsTree(getWordsTree('colorlessgreenideassleepfuriously')));
callAndPrint(() => getOptimalFromWordsTree(getWordsTree('buffalobuffalobuffalobuffalobuffalobuffalobuffalobuffalo')));

const frankenstein = fs.readFileSync(`${__dirname}/frankenstein.txt`).toString().replace(/\s+/g, '').toLocaleLowerCase();
callAndPrint(() => getOptimalFromWordsTree(getWordsTree(frankenstein)).slice(0, 10000) + '...');