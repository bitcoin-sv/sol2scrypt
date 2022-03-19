contract MappingTest {
  mapping(uint256 => uint256) m1;

  mapping(uint256 => mapping (uint256 => uint256)) m2;

  function f1(uint256 a) external {
    m1[a] = 1;
    m1[a] = 2;
    uint256 b = 2;
    m1[a] = b;
    m1[b] = a;
  } 

  function f2(uint256 a) external {
    m2[a][a] = 1;
    uint256 b = a;
    m2[a][b] = a + 1;
    m2[b][a] = b + 1;
  }
}