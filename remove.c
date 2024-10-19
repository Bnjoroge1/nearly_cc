struct Point {
    int x;
    int y;
};

struct Rectangle {
    struct Point top_left;
    struct Point bottom_right;
};

struct Point p1;
struct Rectangle rect;

struct Array {
    int data[2][2];
};
struct Array p2[2][2];