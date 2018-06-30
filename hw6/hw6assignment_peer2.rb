# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
                   rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
                   [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
                    [[0, 0], [0, -1], [0, 1], [0, 2]]],
                   rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
                   rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
                   rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
                   rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
                   rotations([[0, 0], [-1, 0], [-1, -1], [0, -1], [1, 0]]), # 1
                   rotations([[0, 0], [1, 0], [2, 0], [-1, 0], [-2, 0]]), #-----
                   rotations([[0, 0], [0, -1], [1, 0]])] # triangle
  Cheat_Piece = [[[[0, 0]]]] # cheat piece

  # your enhancements here
  def initialize (point_array, board)
    super(point_array, board)
  end

  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.cheat_piece (board)
    MyPiece.new(Cheat_Piece.sample, board)
  end
  
end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
    @no_cheat = true
  end

  def rotate_clockwise_half
    if !game_over? and @game.is_running?
      @current_block.move(0,0,2)
    end
    draw
  end

  # change next piece
  # if 'c' is hit, current_piece change to cheat piece
  def next_piece
    if !@no_cheat
      @current_block = MyPiece.cheat_piece(self)
      @current_pos = nil
      @no_cheat = true
    else
      @current_block = MyPiece.next_piece(self)
      @current_pos = nil
    end
  end

  # change the locations size range
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..locations.size-1).each {|index|
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
        @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  # call when 'c' is hit
  # update the score
  def cheat
    if @score >= 100 and @no_cheat
      @score -= 100
      @no_cheat = false
    end
  end
  
end

class MyTetris < Tetris
  # your enhancements here
  def initialize
    super
  end

  # override @board definition
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  # key binds for 'u' and 'c'
  def key_bindings
    super
    @root.bind('u', proc {@board.rotate_clockwise_half})
    @root.bind('c', proc {@board.cheat})
  end
end


