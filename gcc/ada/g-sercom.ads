------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--           G N A T . S E R I A L _ C O M M U N I C A T I O N S            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                       Copyright (C) 2007, AdaCore                        --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Serial communications package, implemented on Windows and GNU/Linux

with Ada.Streams;
with Interfaces.C;

package GNAT.Serial_Communications is

   Serial_Error : exception;
   --  Raised when a communication problem occurs

   type Port_Name is new String;
   --  A serial com port name

   function Name (Number : Positive) return Port_Name;
   --  Returns the port name for the given port number

   type Data_Rate is (B1200, B2400, B4800, B9600, B19200, B38400, B57600);
   --  Speed of the communication

   type Data_Bits is (B8, B7);
   --  Communication bits

   type Serial_Port is new Ada.Streams.Root_Stream_Type with private;

   procedure Open
     (Port : out Serial_Port;
      Name : Port_Name);
   --  Open the given port name. Raises Serial_Error if the port cannot be
   --  opened.

   procedure Set
     (Port    : Serial_Port;
      Rate    : Data_Rate := B9600;
      Bits    : Data_Bits := B8;
      Block   : Boolean   := True;
      Timeout : Integer   := 10);
   --  The communication port settings. If Block is set then a read call
   --  will wait for the whole buffer to be filed. If Block is not set then
   --  the given Timeout (in seconds) is used.

   overriding procedure Read
     (Port   : in out Serial_Port;
      Buffer : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);
   --  Read a set of bytes, put result into Buffer and set Last accordingly.
   --  Last is set to 0 if no byte has been read.

   overriding procedure Write
     (Port   : in out Serial_Port;
      Buffer : Ada.Streams.Stream_Element_Array);
   --  Write buffer into the port

   procedure Close (Port : in out Serial_Port);
   --  Close port

private

   type Port_Data;
   type Port_Data_Access is access Port_Data;

   type Serial_Port is new Ada.Streams.Root_Stream_Type with record
      H : Port_Data_Access;
   end record;

   Data_Rate_Value : constant array (Data_Rate) of Interfaces.C.unsigned :=
                       (B1200  => 1_200,
                        B2400  => 2_400,
                        B4800  => 4_800,
                        B9600  => 9_600,
                        B19200 => 19_200,
                        B38400 => 38_400,
                        B57600 => 57_600);

   Bit_Value : constant array (Data_Bits) of Interfaces.C.unsigned := (8, 7);

end GNAT.Serial_Communications;