class Database
  attr_reader :last_created
  
  def initialize(db)
    @database = db
  end
  
  def save(document)
    response = @database.save_doc(document)
    @last_created = response['id']
  end
  
  def find(query_name, key)
    @database.view("erlooms/#{query_name}", {:key => "#{key}"})["rows"].collect {|row| row["value"]}
  end
  
  def get(id)
    @database.get(id)
  end
end