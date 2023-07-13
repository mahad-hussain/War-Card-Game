defmodule WarTest do
  use ExUnit.Case
  doctest War

test "deal_1" do
  t1 = [9, 6, 12, 13, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 1, 3, 2, 10, 5, 6, 7, 8, 9, 4, 12, 11, 13, 1, 2, 3, 4, 10, 11, 7, 8, 9, 10, 11, 12, 13, 1, 2, 3, 4, 5, 6, 7, 8, 5]
  r1 = [8, 3, 6, 5, 3, 3, 2, 2, 1, 9, 11, 2, 8, 6, 7, 4, 1, 11, 13, 11, 8, 8, 7, 4, 1, 6, 9, 2, 1, 13, 13, 12, 10, 6, 13, 9, 12, 12, 12, 10, 9, 7, 11, 10, 7, 5, 5, 4, 10, 4, 5, 3]
  assert War.deal(t1) == r1
end

test "deal_2" do
  t2 = [1, 10, 1, 1, 1, 10, 10, 10, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 9, 9, 9, 9, 11, 11, 11, 11, 12, 12, 12, 12, 13, 13, 13, 13]
  r2 = [1, 1, 1, 1, 13, 13, 13, 13, 12, 12, 12, 12, 11, 11, 11, 11, 10, 10, 10, 10, 9, 9, 9, 9, 8, 8, 8, 8, 7, 7, 7, 7, 6, 6, 6, 6, 5, 5, 5, 5, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2]
  assert War.deal(t2) == r2
end

test "deal_3" do
  t3 = [1, 6, 6, 11, 12, 3, 10, 1, 12, 4, 5, 7, 12, 13, 12, 13, 9, 7, 8, 11, 7, 6, 2, 10, 7, 10, 8, 3, 1, 4, 9, 10, 5, 8, 3, 11, 8, 9, 2, 5, 2, 5, 13, 4, 9, 1, 4, 2, 11, 13, 3, 6]
  r3 = [4, 3, 6, 3, 1, 3, 12, 2, 11, 10, 7, 5, 12, 11, 13, 11, 11, 10, 8, 4, 1, 4, 13, 4, 5, 2, 13, 9, 12, 8, 10, 3, 5, 2, 1, 9, 1, 6, 8, 6, 10, 9, 7, 6, 13, 8, 12, 7, 9, 7, 5, 2]
  assert War.deal(t3) == r3
end

test "deal_4" do
  t4 = [10, 11, 1, 13, 13, 4, 2, 2, 5, 4, 9, 9, 7, 7, 11, 11, 12, 12, 2, 2, 4, 4, 6, 7, 8, 8, 10, 11, 12, 12, 1, 2, 3, 3, 6, 6, 8, 8, 10, 10, 13, 13, 1, 1, 3, 3, 5, 5, 6, 7, 9, 9]
  r4 = [1, 13, 13, 10, 10, 9, 9, 8, 8, 7, 6, 6, 6, 5, 5, 3, 3, 3, 3, 2, 12, 12, 12, 12, 11, 11, 11, 10, 9, 9, 8, 8, 7, 7, 7, 6, 5, 4, 4, 4, 2, 2, 1, 13, 13, 4, 2, 2, 1, 11, 1, 10]
  assert War.deal(t4) == r4
end

test "deal_5" do
  t5 = [2, 1, 3, 4, 4, 6, 7, 8, 8, 8, 11, 12, 12, 1, 2, 3, 4, 5, 6, 6, 9, 9, 10, 11, 12, 12, 2, 1, 3, 6, 5, 5, 7, 9, 10, 10, 11, 1, 13, 13, 2, 3, 4, 5, 7, 7, 8, 9, 10, 11, 13, 13]
  r5 = [13, 13, 11, 10, 9, 8, 7, 7, 5, 4, 3, 2, 1, 1, 13, 13, 11, 10, 10, 9, 7, 6, 5, 5, 3, 2, 12, 12, 11, 10, 9, 9, 6, 6, 5, 4, 3, 2, 1, 12, 12, 11, 8, 8, 8, 7, 6, 4, 4, 3, 1, 2]
  assert War.deal(t5) == r5
end
end
