# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  # class array holding all the pieces and their rotations
  All_My_Pieces = [[[[0, 0], [-1, 0], [-2, 0], [1, 0], [2, 0]],
                    [[0, 0], [0, -1], [0, -2], [0, 1], [0, 2]]],
                   rotations([[0, 0], [-1, 0], [-1, -1], [0, -1], [1, -1]]),
                   rotations([[0, 0], [0, -1], [1, -1]])] + All_Pieces

  # class method to choose the next piece
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.cheat_piece(board)
    MyPiece.new([[[0, 0]]], board)
  end

end

class MyBoard < Board

  def initialize (game)
    super(game)
    @current_block = MyPiece.next_piece(self)
    @next_cheat_piece = false
  end

  # gets the next piece
  def next_piece
    if @next_cheat_piece
      @current_block = MyPiece.cheat_piece(self)
      @next_cheat_piece = false
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end

  def next_piece_is_cheat_piece
    if @score >= 100 && !@next_cheat_piece
      @next_cheat_piece = true
      @score = @score - 100
    end
  end

  # gets the information from the current piece about where it is and uses this
  # to store the piece on the board itself.  Then calls remove_filled.
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..@current_block.current_rotation.size-1).each{|index|
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
        @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

end

class MyTetris < Tetris
  # creates a canvas and the board that interacts with it
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    @root.bind('u', proc {@board.rotate_clockwise; @board.rotate_clockwise})
    @root.bind('c', proc {@board.next_piece_is_cheat_piece})
  end
end
