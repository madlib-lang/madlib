#include <future>
#include <chrono>
#include <thread>
#include <cmath>


int asyncNum(int timeout) {
  std::this_thread::sleep_for(std::chrono::milliseconds(timeout));
  return timeout;
}


int asyncValue(int constructor(int), int value) {
  auto a1 = std::async(std::launch::async, constructor, value);
  std::this_thread::sleep_for(std::chrono::milliseconds(3000));
  return a1.get();
}


void *__after__(double *millis, void *value) {
  auto a1 = std::async(std::launch::async, [value]() { return value; });
  std::this_thread::sleep_for(std::chrono::milliseconds((long) std::round(*millis)));
  return a1.get();
}


void fulfill(void callback(int), std::future<int> *future) {
  int result = (*future).get();
  callback(result);
}


int main() {
  auto a1 = std::async(std::launch::async, asyncNum, 1000);
  auto a2 = std::async(std::launch::async, asyncNum, 500);
  auto a3 = std::async(std::launch::async, asyncNum, 500);
  auto a4 = std::async(std::launch::async, asyncNum, 500);

  auto printIt = [](int r){
    printf("%d\n", r);
  };


  int av = asyncValue([](int v) { return v; }, 4);
  printf("%d\n", av);


  std::this_thread::sleep_for(std::chrono::milliseconds(500));

  printf("started:\n");
  fulfill(printIt, &a2);
  fulfill(printIt, &a3);
  fulfill(printIt, &a4);
  fulfill(printIt, &a1);

  return 0;
}
