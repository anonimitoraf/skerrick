const sleep = ms => new Promise(resolve => setTimeout(resolve, ms));
(async () => {
  await sleep(2000);
  console.log('after some sleep');
  return 42;
})();
