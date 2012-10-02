# prerequisites:
#   gem install em-websocket-server json
#
# run with:
#   ruby server.rb

require 'rubygems'
require 'em-websocket'
require 'json'

Room = Struct.new(:room_id, :name, :lat, :lng, :member_count)
class Struct
  def to_map
    map = Hash.new
    self.members.each { |m| map[m] = self[m] }
    map
  end

  def to_json(*a)
    to_map.to_json(*a)
  end
end

def now_time
  Time.now.strftime("%H:%M:%S")
end

EventMachine.run do
  
  # create a chatroom object. Shared by every connection
  @chatroom = EM::Channel.new

  # local rooms { id => { :name => "Boston", :lat => "100", :lng => "-100", :member_count => 1, :channel => EM::Channel} }
  @rooms = {}

  # { :user_id => :room_id }
  @memberships = {}

  EventMachine::WebSocket.start(:host => "0.0.0.0", :port => 9394, :debug => true) do |ws|

    # fires when we open a connection
    ws.onopen do
      
      # holds info about the user's chat session. 
      chatsession = {:nick => "GuestUser"}
      
      # subscribe to the chatroom
      sid = @chatroom.subscribe do |msg|
        ws.send msg 
      end
      
      @chatroom.push({time: now_time, msg: "#{chatsession[:nick]} joined!"}.to_json)
      
      # fires when we receive a message on the channel
      ws.onmessage do |msg|
        
        if msg && msg[0] &&  msg[0].chr == "/"
          parse_command(ws, msg, chatsession)
        else
          @chatroom.push({room_id: @memberships[sid], nick: chatsession[:nick], time: now_time, msg:msg}.to_json)
        end
      end
      
      # fires when someone leaves
      ws.onclose do
        @chatroom.push({time: now_time, msg: "#{chatsession[:nick]} has left!"}.to_json)
        @chatroom.unsubscribe(sid)
      end
      
      # command parser
      #
      # /rooms
      #   { "rooms" : [ { "room_id":"1", "name":"room_1", "lat":"1", "lng":"1", "member_count":"1" }, ... ] }
      #
      # /create JSON {name:name, lat:lat, lng:lng}
      #   { "room" : { "room_id":"1", "name":"room_1", "lat":"1", "lng":"1", "member_count":"1" } }
      # Also notifies users of the new room:
      #   { "room" : { ... } }
      #
      # /enter room_id
      #
      # /exit room_id
      #
      def parse_command(ws, msg, chatsession)
        args = msg.split(/\s+/, 3)[0]
        command = args[0]
        
        case command
        when "/rooms"
          rooms = []
          @rooms.each do |room_id,v|
            rooms += [Room.new(room_id, v[:name], v[:lat], v[:lng], v[:member_count])]
          end

          msg = %Q@{"rooms":#{rooms.collect { |r| r.to_json }}}@

          ws.send msg

          #ws.send '{ "rooms" : [
          #             {"room_id":"1", "name":"room_1", "lat":"111", "lng""111", "member_count":"1"},
          #             {"room_id":"5", "name":"room_5", "lat":"55", "lng":"55", "member_count":"5"},
          #             {"room_id":"12", "name":"room_12", "lat":"12", "lng":"12", "member_count":"12"}
          #           ] }'

        when "/create_room"

          puts "### create"
          puts parts.inspect

          room_name = parts[0]
          lat = parts[1]
          lng = parts[2]

          room_id = @rooms.size
          @rooms[room_id] = { :room_id => room_id, :name => room_name, :lat => lat, :lng => lng,  :member_count => 1, :channel => EM::Channel.new }

          # Notify all clients of new room
          room = Room.new(room_id, room_name, lat, lng, 1)
          @chatroom.push room.to_json
        when "/enter"
          puts "### enter"

          room_id = args[1]
          room = @rooms[room_id]

          # Todo: if !room

          # Subscribe to the channel
          channel = room[:channel]
          sid = channel.subscribe do |msg|
            ws.send msg
          end
          room[:member_count] += 1

          @memberships[sid] = room_id.to_i

          # Reply
          ws.send "Changed your room to #{room[:name]}"
          channel.push "#{chatsession[:nick]} has joined the room"
        when "/exit"
          puts "### exit"
          puts options

          room_id = options
          room = @rooms[room_id]

          # Unsubscribe
          channel = room[:channel]
          # TODO: channel.unsubscribe(sid)
          room[:member_count] -= 1

          # Reply
          ws.send "You have exited room #{room_id}"
          @chatroom.push "User #{chatsession[:nick]} has left"
        when "nick"
          oldnick = chatsession[:nick]
          chatsession[:nick] = options
          ws.send "Changed your nick to #{chatsession[:nick]}."
          @chatroom.push "#{oldnick} is now known as #{chatsession[:nick]}"
        end
      end

    end
  end

  puts "Chat server started"
end

class Room
end

