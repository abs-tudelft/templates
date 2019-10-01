library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity one_process_example is
  port (
    clk   : in  std_logic;
    reset : in  std_logic;

    in_valid  : in  std_logic;
    in_ready  : out std_logic;
    in_data   : in  std_logic_vector(7 downto 0);

    out_valid : out std_logic;
    out_ready : in  std_logic;
    out_data  : out std_logic_vector(7 downto 0)
  );
end one_process_example;

-- The easiest way to store state in this one-process methedology (read,
-- the cleanest code, with the least amount of repetition) is to use only
-- variables. This does have two downsides:
--  - variables cannot be logged in almost any simulator;
--  - synthesis tools tend to make a mess of their internal signal naming,
--    so critical path traces and the likes are harder to debug;
--  - since you're progressively updating the state variables in this
--    methedology, you don't automatically have access to the unmodified
--    register output anymore in the middle or end of the process. It's
--    almost always logically sound to use the updated (combinatorial) state
--    signal, but this may increase logic depth unnecessarily if you don't
--    have to use the updated version.
-- You can solve these things by making a signal copy of the state, ideally
-- through a record type defined in the architecture, that you assign to a
-- variable at the start of the process and vice versa at the end. The
-- downside is that every state variable gets a prefix, so it clutters up
-- the code. The second architecture uses such a signal.
architecture example_1 of one_process_example is
begin
  proc: process (clk) is

    -- Each stream going into or out of a one-process stream operator gets a
    -- holding register, consisting of a valid bit and the data/control payload
    -- of the stream. In this case, we just have a single 8-bit data vector as
    -- payload, but if we were to include last and such, they would appear here
    -- as well. If all tools you're using support records and you don't have
    -- any generics that affect the sizing of the stream payload, it's a good
    -- idea to put these in a record both on the interface and in the state
    -- variables to reduce clutter even further.
    variable in_hold_valid  : std_logic;
    variable in_hold_data   : std_logic_vector(7 downto 0);

    variable out_hold_valid : std_logic;
    variable out_hold_data  : std_logic_vector(7 downto 0);

  begin
    if rising_edge(clk) then

      -- The holding registers for input streams look like this. Conceptually,
      -- if the holding register was empty, than we put whatever comes in into
      -- it. That might be invalid data as well, in which case the holding
      -- register remains empty due to `in_hold_valid := in_valid`. Note that
      -- this logic is only valid in combination with the ready signal
      -- assignment at the end of the process; in_hold_valid must not be
      -- modified before this conditional block.
      if in_hold_valid = '0' then
        in_hold_valid := in_valid;
        in_hold_data := in_data;
      end if;

      -- Output streams also get a holding register. It is always invalidated
      -- when the ready signal is asserted: if there was something in the
      -- holding register, implying that valid was also asserted, the data is
      -- handshaked and thus conceptually leaves the holding register; if there
      -- was nothing in there, this is no-op.
      if out_ready = '1' then
        out_hold_valid := '0';
      end if;

      -- Operations that you want to perform on the stream should be performed
      -- here, in the middle of the process, when the holding registers for all
      -- input streams that you may want to pop are valid, and the holding
      -- registers for all output streams that you may want to push to are
      -- empty. In this case, we just have one input and one output stream and
      -- make a slice, so the input holding register has to be valid, and the
      -- output holding register has to be empty.
      if in_hold_valid = '1' and out_hold_valid = '0' then

        -- To push to an output stream, simply validate the output holding
        -- register and put data into it. Note that you're only allowed to do
        -- this if the holding register was empty, otherwise you may lose
        -- transfers and/or violate AXI stream requirements. That's why we
        -- checked that the register was empty in the block condition.
        out_hold_valid := '1';
        out_hold_data := in_hold_data;

        -- To pop an input stream, simply invalidate it.
        in_hold_valid := '0';

      end if;

      -- During reset, we just have to clear our holding registers. There is a
      -- small catch here: ready will be asserted during reset. This is fine if
      -- the reset for all stream operators is released in the same cycle, but
      -- when you have clock or reset domain crossings this is not good enough.
      -- The second example listed below uses an additional variable to handle
      -- this situation correctly.
      if reset = '1' then
        in_hold_valid := '0';
        out_hold_valid := '0';
      end if;

      -- The ready signal for input streams must be assigned like this at the
      -- end of the process. in_hold_valid must not be changed after this.
      in_ready <= not in_hold_valid;

      -- Output streams are just assigned to their final holding register
      -- state.
      out_valid <= out_hold_valid;
      out_data <= out_hold_data;

    end if;
  end process;
end example_1;

-- In the second example, we change the following things w.r.t. the first:
--  - the input ready signal goes low during reset;
--  - the state is stored in signals instead of in variables.
architecture example_2 of one_process_example is

  -- We define a record for the state so we don't have to replicate the names
  -- of all state signals twice.
  type state_type is record
    in_hold_ready   : std_logic;
    in_hold_valid   : std_logic;
    in_hold_data    : std_logic_vector(7 downto 0);
    out_hold_valid  : std_logic;
    out_hold_data   : std_logic_vector(7 downto 0);
  end record;

  -- Use a short name for the state record! It should be the only signal
  -- anyway.
  signal r  : state_type;

begin
  proc: process (clk) is

    -- We need to replicate the state as a variable to make this methedology
    -- work. Again, use a short name!
    variable s  : state_type;

  begin
    if rising_edge(clk) then

      -- Copy the signal into the variable at the very start of the process.
      -- Technically, this is not necessary, but it might help with
      -- intermediate signal naming during synthesis.
      s := r;

      -- Only copy the stream into the holding register when we actually
      -- signaled ready, instead of just when the holding register is empty.
      -- This handles the special case to make ready low during reset.
      if s.in_hold_ready = '1' then
        s.in_hold_valid := in_valid;
        s.in_hold_data := in_data;
      end if;

      if out_ready = '1' then
        s.out_hold_valid := '0';
      end if;

      if s.in_hold_valid = '1' and s.out_hold_valid = '0' then
        s.out_hold_valid := '1';
        s.out_hold_data := s.in_hold_data;
        s.in_hold_valid := '0';
      end if;

      -- To have ready be low during reset, we need an additional variable
      -- to store the actual state of the ready signal, versus only the state
      -- of the holding register.
      s.in_hold_ready := not s.in_hold_valid;

      -- During reset, we also clear the ready signal.
      if reset = '1' then
        s.in_hold_ready := '0';
        s.in_hold_valid := '0';
        s.out_hold_valid := '0';
      end if;

      in_ready <= s.in_hold_ready;
      out_valid <= s.out_hold_valid;
      out_data <= s.out_hold_data;

      -- Assign the state signal from the updated state variable.
      r <= s;

    end if;
  end process;
end example_2;
