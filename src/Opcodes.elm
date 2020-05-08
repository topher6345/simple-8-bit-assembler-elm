module Opcodes exposing (add_address_to_reg, add_number_to_reg, add_reg_to_reg, add_regaddress_to_reg, and_address_with_reg, and_number_with_reg, and_reg_with_reg, and_regaddress_with_reg, call_address, call_regaddress, cmp_address_with_reg, cmp_number_with_reg, cmp_reg_with_reg, cmp_regaddress_with_reg, dec_reg, div_address, div_number, div_reg, div_regaddress, inc_reg, ja_address, ja_regaddress, jc_address, jc_regaddress, jmp_address, jmp_regaddress, jna_address, jna_regaddress, jnc_address, jnc_regaddress, jnz_address, jnz_regaddress, jz_address, jz_regaddress, mov_address_to_reg, mov_number_to_address, mov_number_to_reg, mov_number_to_regaddress, mov_reg_to_address, mov_reg_to_reg, mov_reg_to_regaddress, mov_regaddress_to_reg, mul_address, mul_number, mul_reg, mul_regaddress, none, not_reg, or_address_with_reg, or_number_with_reg, or_reg_with_reg, or_regaddress_with_reg, pop_reg, push_address, push_number, push_reg, push_regaddress, ret, shl_address_with_reg, shl_number_with_reg, shl_reg_with_reg, shl_regaddress_with_reg, shr_address_with_reg, shr_number_with_reg, shr_reg_with_reg, shr_regaddress_with_reg, sub_address_from_reg, sub_number_from_reg, sub_reg_from_reg, sub_regaddress_from_reg, xor_address_with_reg, xor_number_with_reg, xor_reg_with_reg, xor_regaddress_with_reg)


none =
    0


mov_reg_to_reg =
    1


mov_address_to_reg =
    2


mov_regaddress_to_reg =
    3


mov_reg_to_address =
    4


mov_reg_to_regaddress =
    5


mov_number_to_reg =
    6


mov_number_to_address =
    7


mov_number_to_regaddress =
    8


add_reg_to_reg =
    10


add_regaddress_to_reg =
    11


add_address_to_reg =
    12


add_number_to_reg =
    13


sub_reg_from_reg =
    14


sub_regaddress_from_reg =
    15


sub_address_from_reg =
    16


sub_number_from_reg =
    17


inc_reg =
    18


dec_reg =
    19


cmp_reg_with_reg =
    20


cmp_regaddress_with_reg =
    21


cmp_address_with_reg =
    22


cmp_number_with_reg =
    23


jmp_regaddress =
    30


jmp_address =
    31


jc_regaddress =
    32


jc_address =
    33


jnc_regaddress =
    34


jnc_address =
    35


jz_regaddress =
    36


jz_address =
    37


jnz_regaddress =
    38


jnz_address =
    39


ja_regaddress =
    40


ja_address =
    41


jna_regaddress =
    42


jna_address =
    43


push_reg =
    50


push_regaddress =
    51


push_address =
    52


push_number =
    53


pop_reg =
    54


call_regaddress =
    55


call_address =
    56


ret =
    57


mul_reg =
    60


mul_regaddress =
    61


mul_address =
    62


mul_number =
    63


div_reg =
    64


div_regaddress =
    65


div_address =
    66


div_number =
    67


and_reg_with_reg =
    70


and_regaddress_with_reg =
    71


and_address_with_reg =
    72


and_number_with_reg =
    73


or_reg_with_reg =
    74


or_regaddress_with_reg =
    75


or_address_with_reg =
    76


or_number_with_reg =
    77


xor_reg_with_reg =
    78


xor_regaddress_with_reg =
    79


xor_address_with_reg =
    80


xor_number_with_reg =
    81


not_reg =
    82


shl_reg_with_reg =
    90


shl_regaddress_with_reg =
    91


shl_address_with_reg =
    92


shl_number_with_reg =
    93


shr_reg_with_reg =
    94


shr_regaddress_with_reg =
    95


shr_address_with_reg =
    96


shr_number_with_reg =
    97
