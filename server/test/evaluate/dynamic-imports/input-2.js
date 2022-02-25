(async () => {
  const p = await import('path');
  const m = await import('./input-1');
  console.log('Built-in imported dynamically', p.basename);
  console.log('Module imported dynamically', m);
  return m;
})();
