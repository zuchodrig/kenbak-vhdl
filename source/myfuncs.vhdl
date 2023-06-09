
library	IEEE;
use			IEEE.std_logic_1164.all;
use			IEEE.numeric_std.all;
use			IEEE.math_real.all;
use			work.utils.all;

package myfuncs is

function raw_format_slv_hex(slv : std_logic_vector) return string;

function to_bstring(slv: std_logic_vector) return string;
function ite(cond : boolean; value1 : std_logic; value2 : std_logic) return std_logic;
function to_char(Value : std_logic) return character;
function to_HexChar(Value : natural)	return character;
function to_HexChar(Value : unsigned)	return character;

end package;

package body myfuncs is


  function descend(vec : std_logic_vector)	return std_logic_vector is
		variable  res : std_logic_vector(vec'high downto vec'low);
	begin
		res := vec;
		return  res;
	end function;


	function ite(cond : boolean; value1 : std_logic_vector; value2 : std_logic_vector) return std_logic_vector is
	begin
		if cond then
			return value1;
		else
			return value2;
		end if;
	end function;


  function move(vec : std_logic_vector; ofs : integer) return std_logic_vector is
    variable res_up : std_logic_vector(vec'low +ofs to     vec'high+ofs);
    variable res_dn : std_logic_vector(vec'high+ofs downto vec'low +ofs);
  begin
    if vec'ascending then
      res_up := vec;
      return  res_up;
    else
      res_dn := vec;
      return  res_dn;
    end if;
  end function;


function movez(vec : std_logic_vector) return std_logic_vector is
 begin
   return  move(vec, -vec'low);
 end function;


	function to_char(Value : std_logic) return character is
	begin
		case Value is
			when 'U' =>			return 'U';
			when 'X' =>			return 'X';
			when '0' =>			return '0';
			when '1' =>			return '1';
			when 'Z' =>			return 'Z';
			when 'W' =>			return 'W';
			when 'L' =>			return 'L';
			when 'H' =>			return 'H';
			when '-' =>			return '-';
			when others =>	return 'X';
		end case;
	end function;


function to_bstring(slv: std_logic_vector) return string is
		variable Value				: std_logic_vector(slv'length - 1 downto 0);
		variable Result				: string(1 to slv'length);
		variable j						: natural;
	begin
		-- convert input slv to a downto ranged vector and normalize range to slv'low = 0
		Value := movez(ite(slv'ascending, descend(slv), slv));
		-- convert each bit to a character
		j := 0;
		for i in Result'reverse_range loop
			Result(i)	:= to_char(Value(j));
			j					:= j + 1;
		end loop;
		return Result;
end function to_bstring;

	function raw_format_slv_hex(slv : std_logic_vector) return string is
		variable Value				: std_logic_vector(4*div_ceil(slv'length, 4) - 1 downto 0);
		variable Digit				: std_logic_vector(3 downto 0);
		variable Result				: string(1 to div_ceil(slv'length, 4));
		variable j						: natural;
	begin
		Value := resize(slv, Value'length);
		j			:= 0;
		for i in Result'reverse_range loop
			Digit			:= Value((j * 4) + 3 downto (j * 4));
			Result(i)	:= to_HexChar(unsigned(Digit));
			j					:= j + 1;
		end loop;
		return Result;
	end function;


	function to_HexChar(Value : natural) return character is
	  constant HEX : string := "0123456789ABCDEF";
	begin
		return ite(Value < 16, HEX(Value+1), 'X');
	end function;

	function to_HexChar(Value : unsigned) return character is
	begin
		return to_HexChar(to_integer(Value));
	end function;


	function ite(cond : boolean; value1 : std_logic; value2 : std_logic) return std_logic is
	begin
		if cond then
			return value1;
		else
			return value2;
		end if;
	end function;


end package body;
