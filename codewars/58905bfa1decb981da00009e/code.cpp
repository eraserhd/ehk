#include <iostream>
#include <optional>
#include <set>
#include <vector>
using namespace std;

enum Direction {
    Up = +1,
    Down = -1,
};

Direction opposite(Direction dir)
{
    switch (dir) {
    case Direction::Up: return Direction::Down;
    case Direction::Down: return Direction::Up;
    }
}

struct Floor {
    int number;
    vector<int> queue;

    Floor(int number, vector<int> const& queue)
      : number(number)
      , queue(queue)
    {}

    bool button_pushed(Direction direction) const
    {
        for (auto destination : queue)
            if ((direction == Direction::Up and destination > number) or
                (direction == Direction::Down and destination < number))
                return true;
        return false;
    }
};

struct Lift {
    vector<Floor> floors;
    int current_floor;
    Direction direction;
    int capacity;
    multiset<int> occupants;

    Lift(const std::vector<std::vector<int>> &queues, int capacity)
      : current_floor(0)
      , direction(Direction::Up)
      , capacity(capacity)
      , occupants{}
    {
        for (int n = 0; n < queues.size(); ++n)
            floors.push_back(Floor(n, queues[n]));
    }

    void load()
    {
        auto& q = floors[current_floor].queue;
        for (auto it = q.begin(); occupants.size() < capacity && it != q.end(); ++it)
        {
            if ((direction == Direction::Up and *it > current_floor) or
                (direction == Direction::Down and *it < current_floor))
            {
                occupants.emplace(*it);
                *it = -1;
            }
        }
        q.erase(remove(q.begin(), q.end(), -1), q.end());
    }

    void unload()
    {
        auto range = occupants.equal_range(current_floor);
        occupants.erase(range.first, range.second);
    }

    optional<int> scan_for_pushed_button(Direction dir, optional<int> start = {}) const
    {
        int i, end, step;
        switch (dir)
        {
        case Direction::Up:
            i = 0;
            end = floors.size();
            step = +1;
            break;
        case Direction::Down:
            i = int(floors.size())-1;
            end = -1;
            step = -1;
            break;
        }
        if (start.has_value())
            i = start.value()+step;
        for (; i != end; i += step)
            if (floors[i].button_pushed(dir))
                return {i};
        return {};
    }

    optional<pair<int, Direction>> find_next_stop()
    {
        if (occupants.empty())
        {
            if (auto button = scan_for_pushed_button(direction, current_floor); button.has_value())
                return {{button.value(), direction}};
            if (auto button = scan_for_pushed_button(opposite(direction)); button.has_value())
                return {{button.value(), opposite(direction)}};
            if (auto button = scan_for_pushed_button(direction); button.has_value())
                return {{button.value(), direction}};
            return {};
        }

        int next_occupant_floor = direction == Direction::Up ? *occupants.begin() : *occupants.rbegin();
        for (int i = current_floor + int(direction); i != next_occupant_floor; i += int(direction))
            if (floors[i].button_pushed(direction))
                return {{i, direction}};
        return {{next_occupant_floor, direction}};
    }

    vector<int> run()
    {
        vector result = {0};
        for (;;)
        {
            load();

            auto next = find_next_stop();
            if (!next.has_value())
                break;

            auto [next_floor, next_direction] = next.value();
            current_floor = next_floor;
            if (result.back() != current_floor)
                result.push_back(current_floor);
            direction = next_direction;

            unload();
        }
        if (result.back() != 0)
            result.push_back(0);
        return result;
    }
};

std::vector<int> the_lift(const std::vector<std::vector<int>> &queues, int capacity)
{
    return Lift(queues, capacity).run();
}

//-----------------------------
#include <string>
int main(int argc, char *argv[])
{
    std::vector<std::vector<int>> queues; std::vector<int> result;
    
    queues = { {}, {}, {5,5,5}, {}, {}, {}, {} };
    result = {0, 2, 5, 0};
    auto ans = the_lift(queues, 5);
    copy(ans.begin(), ans.end(), ostream_iterator<int>(cout, " "));
    cout << endl;

    //Assert::That(the_lift(queues, 5), Equals(result));

    /*
    queues = { {}, {}, {1,1}, {}, {}, {}, {} };
    result = {0, 2, 1, 0};
    Assert::That(the_lift(queues, 5), Equals(result));
    
    queues = { {}, {3}, {4}, {}, {5}, {}, {} };
    result = {0, 1, 2, 3, 4, 5, 0};
    Assert::That(the_lift(queues, 5), Equals(result));
    
    queues = { {}, {0}, {}, {}, {2}, {3}, {} };
    result = {0, 5, 4, 3, 2, 1, 0};
    Assert::That(the_lift(queues, 5), Equals(result));
    */
    return 0;
}
