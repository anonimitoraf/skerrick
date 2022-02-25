(async () => {
  const m = await import('./input-1');
  console.log('Module imported dynamically', m);
  return m;
})();
