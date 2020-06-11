function makeColors<K extends string>(colors: K[]): { [key in K]: string } {
  return colors.reduce((memo, next) => {
    memo[next] = `var(--color-${next})`;
    return memo;
  }, {} as any);
}

export const colors = makeColors([
  "white",
  "deep-blue-light",
  "deep-blue-medium",
  "deep-blue-dark",
  "jade-green-light",
  "radical-red-light",
  "viking-light",
  "golden-rod-light",
]);
