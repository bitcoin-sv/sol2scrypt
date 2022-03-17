contract LoopTest {
  uint constant __LoopCount__0 = 1;
  uint constant __LoopCount__1 = 1;
  
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


  function f5() public returns(uint) {
    uint r = 0;
    for (uint i = 0; i < 10; i++) {
        while (true) {
            i += 1;
            r++;
        }
        if (i == 3) {
            r++;
            continue;
        }
        if (i == 5) {
            break;
        }
        r++;
    }
    return r;
  }


  function f6() public returns(uint) {
    uint r = 0;
    for (uint i = 0; i < 10; i++) {
        if (i == 3) {
            r++;
            if( r > 10)
              continue;
            r++;
        } else {
          return r;
        }
        
        r++;
    }
    return r;
  }

   function f7() public returns(uint) {
    uint r = 0;
    for (uint i = 0; i < 10; i++) {
        r++;
        if(r > 3) {
            continue;
        }

        for (uint i = 0; i < 10; i++) {
          r++;
          if(r > 3) {
            continue;
          } else {
            if( r > 10)
              break;
          }

          r++;
        }

        r++;
    }
    return r;
  }

}