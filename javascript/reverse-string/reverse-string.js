export const reverseString = (string) => {
  return [...new Intl.Segmenter().segment(string)].map(x => x.segment).reverse().join('');
};
