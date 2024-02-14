const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));
(async () => {
  await sleep(1000);
  console.log("after some sleep");
  return 42;
})();
