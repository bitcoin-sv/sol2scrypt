contract LoopTest {
  uint x;

  constructor() {
    for(uint i=0; i<2; i++) {
      x += i;
    }
  }

  function f1() external {
    for(uint i=0; i<10; i++) {
      for(uint j=1; j < i; j+=1) {
        if (i == 4) {
          x += i * j;
        }
      }
    }
  }

  function f2() external {
    uint k = 0;
    for(uint i=0; i<10; i++) {
      uint j = 0;
      do {
        uint k = 1;
        if (k < 0) break; else {k += 3;}
        x += k * 2;
      } while (i > 4);
    }
  }

  function f3() external {
    uint i = 0;
    uint j = 0;
    for(i = j; i<10; i++) {
      while(i > 2) {
        if (j > 5) {
          x++;
          break;
        }
        j+=1;
        x += j;
      }
    }
  }

  function f4() public returns(uint) {
    uint r = 0;
    for(uint i =0; i<10; i++) {
      do {
        r += i;
        if (r>10) {
          return r * 10;
        }
      } while (i < 5);
      if (i>6) {
        continue;
      }
      r += i;
    }
    return r;
  }
}