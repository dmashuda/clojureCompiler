#! /usr/bin/ruby

require 'optparse'

class Instruction
  attr_reader :op

  def initialize(op)
    @op = op
    @props = {}
  end

  def set_prop(prop, value)
    @props[prop] = value
  end

  def get_prop(prop)
    return @props[prop]
  end

  @@field_readers = {
    'C' => lambda {|f| return f.read_byte() },
    'N' => lambda {|f| return f.read_int() },
    's' => lambda {|f| return f.read_str() },
  }

  def self.read(f)
    opcode_num = f.read_byte()
    raise "Failed to read opcode" if opcode_num.nil?

    op = Opcode::ALL[opcode_num]
    raise "Unknown opcode #{opcode_num}" if op.nil?

    ins = Instruction.new(op)

    fields = op.fields
    fieldnames = op.fieldnames

    fieldnames.each_index do |i|
      val = @@field_readers[fields[i,i+1]].call(f)
      ins.set_prop(fieldnames[i], val)
    end

    return ins
  end

  def self.write(f, ins)
    f.write_byte(ins.op.num)

    fieldnames = ins.op.fieldnames

    fieldnames.each do |fieldname|
      case fieldname
      when :iconst, :strconst, :addr, :nargs, :syscall, :nclear, :nlocals, :index
        val = ins.get_prop(fieldname)
        raise "No value set for #{fieldname} property in #{ins.op.get_sym()} instruction" if val.nil?
        f.write_int(val)
      else
        raise "Don't know how to write a #{fieldname} field"
      end
    end
  end
end

class Constant
  attr_reader :kind, :value

  def initialize(kind, value)
    @kind = kind
    @value = value
  end

  @@num_to_kind = {
    0 => :str,
  }

  def self.read(f)
    n = f.read_int()
    kind = @@num_to_kind[n]

    raise "Unknown constant type #{n}" if kind.nil?

    case kind
    when :str
      return Constant.new(:str, f.read_str())
    else
      raise "Don't know how to read a #{kind} constant"
    end
  end

  @@kind_to_num = @@num_to_kind.invert()

  def self.write(f, const)
    kind = const.kind
    num = @@kind_to_num[kind]
    raise "Unknown constant type number for #{kind} constant" if num.nil?
    f.write_int(num)

    case kind
    when :str
      f.write_str(const.value)
    else
      raise "Don't know how to write a #{kind} constant"
    end
  end
end

class Syscall
  attr_reader :name, :syscall_num, :nparms, :execute

  def initialize(name, syscall_num, nparms, execute)
    @name = name
    @syscall_num = syscall_num
    @nparms = nparms
    @execute = execute
  end

  ALL = [
    Syscall.new('$print', 0, 1, lambda {|args, vm| print args[0]; vm.printed = args[0]; 0 }),
    Syscall.new('$println', 1, 1, lambda {|args, vm| puts args[0]; vm.printed = args[0]; 0 }),
  ]

  BY_NAME = Hash[ ALL.map {|syscall| [syscall.name, syscall] } ]
end

class Opcode
  attr_reader :num, :sym, :fields, :fieldnames

  def initialize(sym, *more)
    @sym = sym
    fields, fieldnames = more
    @fields = fields.nil? ? '' : fields
    @fieldnames = fieldnames.nil? ? [] : fieldnames
  end

  def _set_num(num)
    @num = num
  end

  ALL = [
    Opcode.new(:i_nop),
    Opcode.new(:i_ldc_i, 'N', [:iconst]),
    Opcode.new(:i_ldc_str, 'N', [:strconst]),
    Opcode.new(:i_add),
    Opcode.new(:i_sub),
    Opcode.new(:i_mul),
    Opcode.new(:i_div),
    Opcode.new(:i_cmp),
    Opcode.new(:i_je, 'N', [:addr]),
    Opcode.new(:i_jne, 'N', [:addr]),
    Opcode.new(:i_jlt, 'N', [:addr]),
    Opcode.new(:i_jgt, 'N', [:addr]),
    Opcode.new(:i_jlte, 'N', [:addr]),
    Opcode.new(:i_jgte, 'N', [:addr]),
    Opcode.new(:i_jmp, 'N', [:addr]),
    Opcode.new(:i_call, 'N', [:addr]),
    Opcode.new(:i_syscall, 'N', [:syscall]),
    Opcode.new(:i_pop),
    Opcode.new(:i_popn, 'N', [:nclear]),
    Opcode.new(:i_enter, 'NN', [:nargs, :nlocals]),
    Opcode.new(:i_ret),
    Opcode.new(:i_ldarg, 'N', [:index]),
    Opcode.new(:i_ldlocal, 'N', [:index]),
    Opcode.new(:i_stlocal, 'N', [:index]),
    Opcode.new(:i_exp),
    Opcode.new(:i_dup),
  ]

  ALL.each_index {|i| ALL[i]._set_num(i) }

  BY_SYM = Hash[ ALL.map {|op| [op.sym, op] } ]
end

class Executor
  attr_accessor :interactive
  attr_reader :output

  def initialize(exe)
    @exe = exe
    @vm = VirtualMachine.new(exe)
    @interactive = false
    @output = []
  end

  def execute
    if @interactive
      def @vm.padfixed(str, width)
        str = str.to_s
        str = str.gsub("\n", '\n')
        if str.length >= 30
          str = "#{str.slice(0, 27)}..."
        end
        return "#{str}#{' ' * (30-str.length)}"
      end

      def @vm.print_state(executor)
        system("clear")
    
        addr = 0
        exe.instructions.each do |ins|
          print (addr == pc) ? "==> " : "    ";
          print "%03d " % addr
          puts ins.get_prop(:source)
          addr += 1
        end

        executor.output.push(printed) if !printed.nil?

        topframe = framestack.last
    
        puts ''
        pstack = ['Current stack:']
        (1 .. opstack.length).each do |i|
          index = opstack.length - i
          val = opstack[index]
          if !topframe.nil? and index >= topframe.base
            pstack.push("| #{val}")
          else
            pstack.push("  #{val}")
          end
        end
        pout = ['Output:']
        pout.concat(executor.output)
        done = false
        while !done
          if pstack.empty? and pout.empty?
            done = true
          else
            left = pstack.shift or ''
            right = pout.shift or ''
            printf("%s  %s\n", padfixed(left, 30), padfixed(right, 40))
          end
        end
      end
    end
    
    while !@vm.halted?
      if @interactive
        @vm.print_state(self)
        STDIN.gets
      end
      @vm.stepi()
    end
  end
end

class ExeFile
  attr_reader :instructions, :constants

  MAGIC = 0xf00ba555

  def initialize
    @instructions = []
    @constants = []
  end

  def self.read(f)
    exe = ExeFile.new()

    raise "Bad magic number" if f.read_int_unsigned() != MAGIC

    nins = f.read_int()
    nconst = f.read_int()

    (1..nins).each {|i| exe.instructions.push(Instruction.read(f)) }
    (1..nconst).each {|i| exe.constants.push(Constant.read(f)) }
    exe.instructions.each do |ins|
      line = f.read_str()
      ins.set_prop(:source, line)
    end

    return exe
  end

  def self.write(f, exe)
    f.write_int(MAGIC)
    f.write_int(exe.instructions.length)
    f.write_int(exe.constants.length)
    exe.instructions.each {|ins| Instruction.write(f, ins) }
    exe.constants.each {|const| Constant.write(f, const) }
    exe.instructions.each {|ins| f.write_str(ins.get_prop(:source)) }
  end
end


class VirtualMachine
  class Frame
    attr_accessor :base

    def initialize(nargs, nlocals)
      @nargs, @nlocals = nargs, nlocals
    end

    def get_arg(opstack, index)
      raise "Invalid argument index #{index} (frame has #{@nargs} args)" if !(0..@nargs-1).include?(index)
      return opstack[@base + index]
    end

    def get_local(opstack, index)
      raise "Invalid argument index #{index} (frame has #{@nlocals} locals)" if !(0..@nlocals-1).include?(index)
      return opstack[@base + @nargs + 1 + index]
    end

    def set_local(opstack, index, val)
      raise "Invalid argument index #{index} (frame has #{@nlocals} locals)" if !(0..@nlocals-1).include?(index)
      opstack[@base + @nargs + 1 + index] = val
    end

    def enter(opstack)
      @base = opstack.length - @nargs - 1

      raise "Frame base index is negative!" if @base < 0

      (1 .. @nlocals).each do |i|
        opstack.push(0)
      end
    end

    def leave(opstack)
      expected = @base + @nargs + @nlocals + 2
      if opstack.length != expected
        raise "Returning from procedure: operand stack wrong size (is #{opstack.length}, expected #{expected})"
      end
      retval = opstack.pop()

      opstack.popn(@nlocals)

      retaddr = opstack.pop()

      opstack.popn(@nargs)

      opstack.push(retval)

      return retaddr
    end
  end

  attr_reader :exe, :pc, :opstack, :framestack
  attr_accessor :printed

  def initialize(exe)
    @exe = exe
    @opstack = []
    def @opstack.popn(nclear)
      (1 .. nclear).each {|i| pop() }
    end
    @framestack = []
    @pc = 0
    @halted = false

    @opstack.push(-1)
  end

  @@opcode_to_arith_method = {
    :i_add => :+,
    :i_sub => :-,
    :i_mul => :*,
    :i_div => :/,
    :i_exp => :**,
  }

  @@check_comparison_result = {
    :i_je => lambda {|res| res == 0 },
    :i_jne => lambda {|res| res != 0 },
    :i_jlt => lambda {|res| res < 0 },
    :i_jgt => lambda {|res| res > 0 },
    :i_jlte => lambda {|res| res <= 0 },
    :i_jgte => lambda {|res| res >= 0 },
  }

  def stepi
    @printed = nil

    ins = @exe.instructions[@pc]
    raise "No instruction at pc=#{@pc}" if ins.nil?

    nextpc = @pc + 1

    opc = ins.op.sym

    case opc
    when :i_nop
    when :i_ldc_i
      @opstack.push(ins.get_prop(:iconst))
    when :i_ldc_str
      index = ins.get_prop(:strconst)
      const = @exe.constants[index]
      raise "Reference to nonexistent constant #{index}" if const.nil?
      @opstack.push(const.value)
    when :i_add, :i_sub, :i_mul, :i_div, :i_exp
      rhs = @opstack.pop()
      lhs = @opstack.pop()
      result = lhs.send(@@opcode_to_arith_method[opc], rhs)
      @opstack.push(result)
    when :i_cmp
      rhs = @opstack.pop()
      lhs = @opstack.pop()
      @opstack.push(lhs <=> rhs)
    when :i_je, :i_jne, :i_jlt, :i_jgt, :i_jlte, :i_jgte
      res = @opstack.pop()
      if @@check_comparison_result[opc].call(res)
        nextpc = ins.get_prop(:addr)
      end
    when :i_jmp
      nextpc = ins.get_prop(:addr)
    when :i_call
      @opstack.push(@pc + 1)

      nextpc = ins.get_prop(:addr)
    when :i_syscall
      num = ins.get_prop(:syscall)
      sys = Syscall::ALL[num]
      raise "Unknown syscall #{num}" if sys.nil?
      args = []
      (1 .. sys.nparms).each do |i|
        args.unshift(@opstack.pop())
      end
      result = sys.execute.call(args, self)
      @opstack.push(result)
    when :i_pop
      @opstack.pop()
    when :i_popn
      (1 .. ins.get_prop(:nclear)).each do |i|
        @opstack.pop()
      end
    when :i_enter
      frame = Frame.new(ins.get_prop(:nargs), ins.get_prop(:nlocals))
      @framestack.push(frame)
      frame.enter(@opstack)
    when :i_ret
      frame = @framestack.pop()
      raise "Returning from nonexistent procedure at pc=#{@pc}" if frame.nil?
      nextpc = frame.leave(@opstack)
      @halted = @framestack.empty?
    when :i_ldarg
      _with_index(ins) do |frame, index|
        @opstack.push(frame.get_arg(@opstack, index))
      end
    when :i_ldlocal
      _with_index(ins) do |frame, index|
        @opstack.push(frame.get_local(@opstack, index))
      end
    when :i_stlocal
      _with_index(ins) do |frame, index|
        frame.set_local(@opstack, index, @opstack.pop())
      end
    when :i_dup
      top = @opstack[-1]
      @opstack.push(top)
    else
      raise "Unknown opcode: #{op}"
    end

    @pc = nextpc
  end

  def _with_index(ins)
    frame = @framestack.last()
    raise "No stack frame!" if frame.nil?
    index = ins.get_prop(:index)
    yield frame, index
  end

  def halted?
    return @halted
  end
end

class Assembler
  def initialize(f)
    @f = f
    @exe = ExeFile.new()
    @labels = {}
  end

  def assemble
    _read_instructions()
    _resolve_labels()
    _build_constant_pool()
  end

  def get_exe
    return @exe
  end

  def _read_instructions
    index = 0
    
    @f.each_line do |line|
      line.strip!
      if line.empty?
      elsif m = line.match(/^;/)
      elsif m = line.match(/^([A-Za-z_][A-Za-z_0-9]*)\s*:$/)
        @labels[m[1]] = index
      else
        if m = line.match(/^([A-Za-z_][A-Za-z_0-9]*)(\s+(.*))?$/)
          sym = "i_#{m[1]}".to_sym()
          op = Opcode::BY_SYM[sym]
          raise "Unknown mnemonic #{m[1]}" if op.nil?
    
          if (m[2].nil?)
            args = []
          else
            args = _parse_args(m[2])
          end
    
          ins = Instruction.new(op)
          ins.set_prop(:source, line)
          index += 1
    
          _handle_args(ins, args)
    
          @exe.instructions.push(ins)
        else
          raise "Syntax error: #{line}"
        end
      end
    end
  end

  def _parse_args(argstr)
    args = []

    argstr.lstrip!()

    while true
      return args if (argstr.strip().empty?)

      if m = argstr.match(/^(-?[0-9]+)(.*)$/)
        args.push(m[1].to_i)
        argstr = m[2]
      elsif m = argstr.match(/^("(\\.|[^"])*")(.*)$/)
        args.push(_handle_escapes(m[1].slice(1, m[1].length - 2)))
        argstr = m[3]
      elsif m = argstr.match(/^(\$[a-z]+)(.*)$/)
        args.push(m[1])
        argstr = m[2]
      elsif m = argstr.match(/^([A-Za-z_][A-Za-z_0-9]*)(.*)$/)
        args.push(m[1])
        argstr = m[2]
      else
        raise "Unrecognized argument: #{argstr}"
      end

      argstr.lstrip!()

      argstr = '' if argstr.match(/^;/)

      if !argstr.empty?
        raise "Arguments must be separated by commas: #{argstr}" if !(m = argstr.match(/^,(.*)$/))
        argstr = m[1]
        argstr.lstrip!()
      end
    end
  end

  @@escapes = {
    'n' => "\n",
    't' => "\t",
    'r' => "\r",
    'b' => "\b",
    'f' => "\f",
    '\\' => "\\",
  }

  def _handle_escapes(s)
    result = ''
    state = :scan
    s.each_char do |c|
      case state
      when :scan
        if c == '\\'
          state = :escape
        else
          result << c
        end
      when :escape
        ch = @@escapes[c]
        raise "Unknown escape sequence: \\#{c}" if ch.nil?
        result << ch
        state = :scan
      end
    end
    return result
  end

  def _handle_args(ins, args)
    op = ins.op
    nparms = op.fields.length

    unless args.length == nparms
      raise "Wrong number of args for #{ins.op.sym} (got #{args.length}, expected #{nparms})"
    end
  
    op = ins.op
    
    (0 .. nparms-1).each do |i|
      field = op.fields[i]
      fieldname = op.fieldnames[i]
  
      case fieldname
      when :iconst, :nargs, :nclear, :nlocals, :index
        ins.set_prop(fieldname, args[i].to_i())
      when :strconst
        ins.set_prop(:strconst_val, args[i])
      when :addr
        ins.set_prop(:target_label, args[i])
      when :syscall
        syscall = Syscall::BY_NAME[args[i]]
        raise "Unknown syscall: #{args[i]}" if syscall.nil?
        ins.set_prop(:syscall, syscall.syscall_num)
      else
        raise "Don't know how to handle #{fieldname} arg"
      end
    end
  end

  def _resolve_labels
    @exe.instructions.each do |ins|
      target_label = ins.get_prop(:target_label)
      if !target_label.nil?
        target_addr = @labels[target_label]
        raise "Label #{target_label} is not defined" if target_addr.nil?
        ins.set_prop(:addr, target_addr)
      end
    end
  end

  def _build_constant_pool
    pool = {}
    @exe.instructions.each do |ins|
      if val = ins.get_prop(:strconst_val)
        index = pool[val]
        if index.nil?
          const = Constant.new(:str, val)
          index = @exe.constants.length
          @exe.constants.push(const)
          pool[val] = index
        end
        ins.set_prop(:strconst, index)
      end
    end
  end
end

mode = :execute
outfile = nil
interactive = false
optparse = OptionParser.new do |opts|
  opts.banner =  "Usage: MiniVM.rb [options] <filename>"

  opts.on('-a', '--assemble', 'translate assembly code to executable') do
    mode = :assemble
  end
  opts.on('-x', '--execute', 'execute assembly program or executable') do
    mode = :execute
  end
  opts.on('-o', '--output <name>', String, 'specify output file name') do |name|
    outfile = name
  end
  opts.on('-i', '--interactive', 'interactive execution') do
    interactive = true
  end
end

optparse.parse!
if ARGV.length != 1
  puts optparse
  exit 1
end
filename = ARGV[0]

m = /^(.*)(\.[^\.]+)$/.match(filename)
raise "Source file must have a file extension" if !m
base = m[1]
ext = m[2]

exe = nil
case ext
when '.mvm'
  File.open(filename) do |f|
    a = Assembler.new(f)
    a.assemble()
    f.close()
    exe = a.get_exe()
  end
when '.mve'
  raise "Nothing to do" unless mode == :execute
  exe = ExeFile.read(filename)
else
  raise "Unknown file extension #{ext}"
end

if mode == :execute
  x = Executor.new(exe)
  x.interactive = interactive
  x.execute()
else
  if outfile.nil?
    outfile = "#{base}.mve"
  end

  outf = File.open(outfile, 'w')
  outf.extend(BinaryFile)
  ExeFile.write(outf, exe)
  outf.close()
end

