# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = [
    rotations([[0, 0], [0, 1], [0, 2], [1, 0], [1, 1]]), # <-- "fat" L
    [[[0, 0], [1, 0], [2, 0], [-1, 0], [-2, 0]],
     [[0, 0], [0, 1], [0, 2], [0, -1], [0, -2]]], # <-- long piece of size 5
    rotations([[0, 0], [1, 0], [0, 1]]) # <-- small L
  ] + All_Pieces

  # your enhancements here
  def self.next_piece(board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.cheat_piece(board)
    MyPiece.new([[[0, 0]]], board)
  end

end

class MyBoard < Board
  # your enhancements here

  def initialize(game)
    super(game)
    @current_block = MyPiece.next_piece(self)
    @cheat_enabled = false
  end

  def cheat_enabled=(enable)
    if enable
      if !@cheat_enabled and (@score >= Cheat_costs)
        @cheat_enabled = enable
        @score -= Cheat_costs
      end
    else
      @cheat_enabled = enable
    end
  end

  def next_piece
    unless @cheat_enabled
      @current_block = MyPiece.next_piece(self)
    else
      @current_block = MyPiece.cheat_piece(self)
      @cheat_enabled = false
    end
    @current_pos = nil
  end

  # store_current needs to be overridden to
  # adapt for pieces with sizes other than 4
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0...locations.length).each{|index|
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  private

  Cheat_costs = 100

end

class MyTetris < Tetris
  # your enhancements here
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    @root.bind('u', proc { 2.times { @board.rotate_clockwise } })
    @root.bind('c', proc { @board.cheat_enabled = true })
  end

end
