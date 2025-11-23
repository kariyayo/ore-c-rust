struct person {
    char *name;
    int age;
};
int main() {
    struct person a;
    a->age = 10;
    return 0;
}
