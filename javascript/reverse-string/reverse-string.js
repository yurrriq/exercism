export const reverseString = (string) => {
  return [...new Intl.Segmenter({ granularity: "grapheme" }).segment(string)]
    .map((x) => x.segment)
    .reverse()
    .join("");
};
