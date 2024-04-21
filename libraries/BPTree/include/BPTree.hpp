#ifndef BPTREE_HPP
#define BPTREE_HPP

#include <cstddef>
#include <functional>
#include <iostream>

template <class Key, class Value, std::size_t BlockSize = 4096, class Less = std::less<Key>>
class BPTree;

template <class Key, class Value, std::size_t BlockSize = 4096, class Less = std::less<Key>>
class Node {
public:
    virtual ~Node() {}
    [[nodiscard]] virtual bool is_leaf() const noexcept     = 0;
    [[nodiscard]] virtual std::size_t size() const noexcept = 0;
    [[nodiscard]] virtual bool is_full() const noexcept     = 0;
};

template <class Key, class Value, std::size_t BlockSize = 4096, class Less = std::less<Key>>
class Internal: public Node<Key, Value, BlockSize, Less> {
public:
    Internal() = default;

    explicit Internal(const Internal &other) {
        m_count_keys = other.m_count_keys;
        for (std::size_t i = 0; i < m_count_keys; i++) {
            m_keys[i] = other.m_keys[i];
        }
    }

    explicit Internal(const Key &key, Node<Key, Value, BlockSize, Less> *first_child,
                      Node<Key, Value, BlockSize, Less> *second_child) {
        m_count_keys  = 1;
        m_keys[0]     = key;
        m_children[0] = first_child;
        m_children[1] = second_child;
    }

    Internal &operator=(const Internal &other) {
        m_count_keys = other.m_count_keys;
        for (std::size_t i = 0; i < m_count_keys; i++) {
            m_keys[i]     = other.m_keys[i];
            m_children[i] = other.m_children[i];
        }
        m_children[m_count_keys] = other.m_children[m_count_keys];
    }

    ~Internal() {
        for (std::size_t i = 0; i <= m_count_keys; i++) {
            delete m_children[i];
        }
    }

    [[nodiscard]] bool is_leaf() const noexcept override { return false; }
    [[nodiscard]] std::size_t size() const noexcept override { return m_count_keys; }
    [[nodiscard]] bool is_full() const noexcept override { return m_count_keys == K; }

private:
    friend class BPTree<Key, Value, BlockSize, Less>;

    static constexpr std::size_t K =
        (BlockSize - sizeof(std::size_t) - sizeof(void *)) / (sizeof(Key) + sizeof(void *));
    std::size_t m_count_keys = 0;
    std::array<Key, K> m_keys;
    std::array<Node<Key, Value, BlockSize, Less> *, K + 1> m_children;
};

template <class Key, class Value, std::size_t BlockSize = 4096, class Less = std::less<Key>>
class Leaf: public Node<Key, Value, BlockSize, Less> {
public:
    Leaf() = default;

    explicit Leaf(const Leaf &other) {
        m_count_keys = other.m_count_keys;
        for (std::size_t i = 0; i < m_count_keys; i++) {
            m_data[i].first  = other.m_data[i].first;
            m_data[i].second = other.m_data[i].second;
        }
    }

    explicit Leaf(const Key &key, const Value &value, Leaf *next) {
        m_count_keys = 1;
        m_data[0]    = std::make_pair(key, value);
        m_next       = next;
    }
    explicit Leaf(const Key &key, Value &&value, Leaf *next) {
        m_count_keys = 1;
        m_data[0]    = std::make_pair(key, std::move(value));
        m_next       = next;
    }

    Leaf &operator=(const Leaf &other) {
        m_next       = other.m_next;
        m_count_keys = other.m_count_keys;
        for (std::size_t i = 0; i < m_count_keys; i++) {
            m_data[i] = other.m_data[i];
        }
    }

    [[nodiscard]] bool is_leaf() const noexcept override { return true; }
    [[nodiscard]] std::size_t size() const noexcept override { return m_count_keys; }
    [[nodiscard]] bool is_full() const noexcept override { return m_count_keys == K; }

private:
    friend class BPTree<Key, Value, BlockSize, Less>;

    static constexpr std::size_t K =
        (BlockSize - sizeof(std::size_t) - sizeof(void *)) / (sizeof(Key) + sizeof(void *));
    std::size_t m_count_keys = 0;
    Leaf *m_next;
    std::array<std::pair<Key, Value>, K> m_data;
};

template <class Key, class Value, std::size_t BlockSize, class Less>
class BPTree: private Less {
public:
    using ValueType = std::pair<Key, Value>;
    using node      = Node<Key, Value, BlockSize, Less>;
    using leaf      = Leaf<Key, Value, BlockSize, Less>;
    using internal  = Internal<Key, Value, BlockSize, Less>;

    template <typename R>
    class Iterator {
    public:
        using iterator_category = std::forward_iterator_tag;
        using difference_type   = std::ptrdiff_t;
        using value_type        = std::conditional_t<std::is_const_v<R>, const ValueType, ValueType>;
        using pointer           = value_type *;
        using reference         = value_type &;

        Iterator() : m_pos(0), m_curr_leaf(nullptr) {}

        explicit Iterator(std::size_t pos, leaf *curr) : m_pos(pos), m_curr_leaf(curr) {}

        template <typename Q>
        Iterator(const Iterator<Q> &I, typename std::enable_if<std::is_same<const Q, R>::value>::type * = nullptr)
            : m_pos(I.m_pos) {
            m_curr_leaf = I.m_curr_leaf;
        }

        reference operator*() const { return m_curr_leaf->m_data[m_pos]; }
        pointer operator->() const { return &m_curr_leaf->m_data[m_pos]; }

        Iterator &operator++() {
            plus_one();
            return *this;
        }

        Iterator operator++(int) {
            Iterator tmp = *this;
            plus_one();
            return tmp;
        }

        friend bool operator==(const Iterator &a, const Iterator &b) noexcept {
            return a.m_curr_leaf == b.m_curr_leaf && a.m_pos == b.m_pos;
        };
        friend bool operator!=(const Iterator &a, const Iterator &b) noexcept { return not(a == b); };

    private:
        void plus_one() {
            if (m_pos < m_curr_leaf->m_count_keys - 1) {
                m_pos++;
            } else if (m_curr_leaf->m_next != nullptr) {
                m_curr_leaf = m_curr_leaf->m_next;
                m_pos       = 0;
            } else {
                m_pos = m_curr_leaf->m_count_keys;
            }
        }

        friend class BPTree;

        std::size_t m_pos;
        R *m_curr_leaf;
    };

    using key_type        = Key;
    using mapped_type     = Value;
    using value_type      = std::pair<Key, Value>;  // NB: a digression from std::map
    using reference       = value_type &;
    using const_reference = const value_type &;
    using pointer         = value_type *;
    using const_pointer   = const value_type *;
    using size_type       = std::size_t;

    using iterator       = Iterator<leaf>;
    using const_iterator = Iterator<leaf const>;

    BPTree();
    BPTree(std::initializer_list<std::pair<Key, Value>> list);
    BPTree(const BPTree &other);
    BPTree(BPTree &&other);
    BPTree<Key, Value, BlockSize, Less> &operator=(const BPTree &other);
    BPTree<Key, Value, BlockSize, Less> &operator=(BPTree &&other);
    ~BPTree();

    iterator begin();
    const_iterator cbegin() const;
    const_iterator begin() const;
    iterator end();
    const_iterator cend() const;
    const_iterator end() const;

    [[nodiscard]] bool empty() const noexcept;
    [[nodiscard]] size_type size() const noexcept;
    void clear();

    [[nodiscard]] size_type count(const Key &key) const noexcept;
    [[nodiscard]] bool contains(const Key &key) const noexcept;
    [[nodiscard]] std::pair<iterator, iterator> equal_range(const Key &key);
    [[nodiscard]] std::pair<const_iterator, const_iterator> equal_range(const Key &key) const;
    [[nodiscard]] iterator lower_bound(const Key &key);
    [[nodiscard]] const_iterator lower_bound(const Key &key) const;
    [[nodiscard]] iterator upper_bound(const Key &key);
    [[nodiscard]] const_iterator upper_bound(const Key &) const;
    [[nodiscard]] iterator find(const Key &key);
    [[nodiscard]] const_iterator find(const Key &key) const;

    // 'at' method throws std::out_of_range if there is no such key
    Value &at(const Key &key);
    const Value &at(const Key &key) const;

    // '[]' operator inserts a new element if there is no such key
    Value &operator[](const Key &);

    std::pair<iterator, bool> insert(const Key &key, const Value &value);  // NB: a digression from std::map
    std::pair<iterator, bool> insert(const Key &key, Value &&value);       // NB: a digression from std::map
    template <class ForwardIt>
    void insert(ForwardIt begin, ForwardIt end);
    void insert(std::initializer_list<value_type> list);
    iterator erase(const_iterator it);
    iterator erase(const_iterator start, const_iterator end);
    size_type erase(const Key &key);

private:
    [[nodiscard]] bool strict_compare(const Key &lhs, const Key &rhs) const noexcept;
    [[nodiscard]] bool equality(const Key &lhs, const Key &rhs) const noexcept;
    [[nodiscard]] bool non_strict_compare(const Key &lhs, const Key &rhs) const noexcept;

    [[nodiscard]] node *copy_tree(node *other);

    [[nodiscard]] leaf *get_left_leaf(node *curr) const noexcept;
    [[nodiscard]] leaf *get_right_leaf(node *curr) const noexcept;
    [[nodiscard]] std::size_t get_child_index(node *parent, node *child) const noexcept;
    [[nodiscard]] leaf *get_next_leaf(leaf *curr_leaf, node *curr_root) const noexcept;
    void set_leaf_connections(node *curr_root) noexcept;

    [[nodiscard]] leaf *find_leaf(node *start, const Key &key) const noexcept;
    [[nodiscard]] std::size_t find_index(node *curr, const Key &key) const noexcept;
    [[nodiscard]] internal *find_parent(node *child, node *curr_root) const noexcept;
    [[nodiscard]] bool strict_compare_with_key(node *curr, std::size_t pos, const Key &key) const noexcept;

    [[nodiscard]] std::pair<std::size_t, leaf *> m_find(const Key &key) const noexcept;
    [[nodiscard]] std::pair<std::size_t, leaf *> m_at(const Key &key) const;

    void shift_left(leaf *curr, std::size_t start, std::size_t end);
    void shift_left(internal *curr, std::size_t start, std::size_t end);
    void shift_right(leaf *curr, std::size_t start, std::size_t end);
    void shift_right(internal *curr, std::size_t start, std::size_t end, std::size_t param);

    void move_leaf(leaf *from, leaf *to, std::size_t start, std::size_t end, std::size_t i_from, std::size_t i_to);
    void move_internal(internal *from, internal *to, std::size_t border);

    [[nodiscard]] std::pair<std::pair<std::size_t, leaf *>, bool> m_insert(const Key &key, const Value &value);
    [[nodiscard]] std::pair<std::pair<std::size_t, leaf *>, bool> m_insert(const Key &key, Value &&value);
    [[nodiscard]] std::pair<std::pair<std::size_t, leaf *>, bool> check_leaf(leaf *curr, const Key &key,
                                                                             std::size_t pos);
    void split_leaf(leaf *curr);
    void insert_internal(internal *curr, const Key &key, node *new_child);
    void check_internal(node *curr, node *new_node, const Key &border_key);
    void split_internal(internal *curr);

    void simple_erase(leaf *curr, internal *parent, std::size_t pos);
    [[nodiscard]] std::pair<std::pair<std::size_t, leaf *>, bool> m_erase(leaf *curr, std::size_t pos, const Key &key);
    void merge_leaf(leaf *left, leaf *right, bool left_leaf, std::size_t pos);
    void erase_internal(internal *curr, std::size_t index);
    void merge_internal(internal *curr);

    [[nodiscard]] std::pair<std::size_t, leaf *> m_lower_bound(const Key &key) const noexcept;
    [[nodiscard]] std::pair<std::size_t, leaf *> m_upper_bound(const Key &key) const noexcept;

    node *root;
};

template <class Key, class Value, std::size_t BlockSize, class Less>
bool BPTree<Key, Value, BlockSize, Less>::strict_compare(const Key &lhs, const Key &rhs) const noexcept {
    return Less::operator()(lhs, rhs);
}

template <class Key, class Value, std::size_t BlockSize, class Less>
bool BPTree<Key, Value, BlockSize, Less>::equality(const Key &lhs, const Key &rhs) const noexcept {
    return (Less::operator()(lhs, rhs) == false) && (Less::operator()(rhs, lhs) == false);
}

template <class Key, class Value, std::size_t BlockSize, class Less>
bool BPTree<Key, Value, BlockSize, Less>::non_strict_compare(const Key &lhs, const Key &rhs) const noexcept {
    return strict_compare(lhs, rhs) || equality(lhs, rhs);
}

template <class Key, class Value, std::size_t BlockSize, class Less>
BPTree<Key, Value, BlockSize, Less>::BPTree() {
    root = nullptr;
}

template <class Key, class Value, std::size_t BlockSize, class Less>
BPTree<Key, Value, BlockSize, Less>::BPTree(std::initializer_list<std::pair<Key, Value>> list) {
    root    = nullptr;
    auto it = list.begin();
    while (it != list.end()) {
        insert(it->first, it->second);
        it++;
    }
}

template <class Key, class Value, std::size_t BlockSize, class Less>
BPTree<Key, Value, BlockSize, Less>::BPTree(const BPTree &other) {
    if (other.empty()) {
        root = nullptr;
    } else {
        root = copy_tree(other.root);
        set_leaf_connections(root);
    }
}

template <class Key, class Value, std::size_t BlockSize, class Less>
BPTree<Key, Value, BlockSize, Less>::BPTree(BPTree &&other) {
    root       = other.root;
    other.root = nullptr;
}

template <class Key, class Value, std::size_t BlockSize, class Less>
BPTree<Key, Value, BlockSize, Less> &BPTree<Key, Value, BlockSize, Less>::operator=(
    const BPTree<Key, Value, BlockSize, Less> &other) {
    if (this != &other) {
        if (not empty()) {
            clear();
        }
        if (not other.empty()) {
            root = copy_tree(other.root);
            set_leaf_connections(root);
        }
    }
    return *this;
}

template <class Key, class Value, std::size_t BlockSize, class Less>
BPTree<Key, Value, BlockSize, Less> &BPTree<Key, Value, BlockSize, Less>::operator=(
    BPTree<Key, Value, BlockSize, Less> &&other) {
    clear();
    root       = other.root;
    other.root = nullptr;
    return *this;
}

template <class Key, class Value, std::size_t BlockSize, class Less>
BPTree<Key, Value, BlockSize, Less>::~BPTree() {
    delete root;
}

template <class Key, class Value, std::size_t BlockSize, class Less>
Node<Key, Value, BlockSize, Less> *BPTree<Key, Value, BlockSize, Less>::copy_tree(BPTree::node *other) {
    if (other->is_leaf()) {
        leaf *new_node = new leaf(*static_cast<leaf *>(other));
        return new_node;
    } else {
        internal *new_node = new internal(*static_cast<internal *>(other));
        for (std::size_t i = 0; i <= other->size(); i++) {
            new_node->m_children[i] = copy_tree(static_cast<internal *>(other)->m_children[i]);
        }
        return new_node;
    }
}

template <class Key, class Value, std::size_t BlockSize, class Less>
typename BPTree<Key, Value, BlockSize, Less>::iterator BPTree<Key, Value, BlockSize, Less>::begin() {
    if (root == nullptr) {
        return iterator(0, nullptr);
    }
    return iterator(0, get_left_leaf(root));
}

template <class Key, class Value, std::size_t BlockSize, class Less>
typename BPTree<Key, Value, BlockSize, Less>::const_iterator BPTree<Key, Value, BlockSize, Less>::cbegin() const {
    if (root == nullptr) {
        return const_iterator(0, nullptr);
    }
    return const_iterator(0, get_left_leaf(root));
}

template <class Key, class Value, std::size_t BlockSize, class Less>
typename BPTree<Key, Value, BlockSize, Less>::const_iterator BPTree<Key, Value, BlockSize, Less>::begin() const {
    return cbegin();
}

template <class Key, class Value, std::size_t BlockSize, class Less>
typename BPTree<Key, Value, BlockSize, Less>::iterator BPTree<Key, Value, BlockSize, Less>::end() {
    if (root == nullptr) {
        return iterator(0, nullptr);
    }
    leaf *right = get_right_leaf(root);
    return iterator(right->size(), right);
}

template <class Key, class Value, std::size_t BlockSize, class Less>
typename BPTree<Key, Value, BlockSize, Less>::const_iterator BPTree<Key, Value, BlockSize, Less>::cend() const {
    if (root == nullptr) {
        return const_iterator(0, nullptr);
    }
    leaf *right = get_right_leaf(root);
    return const_iterator(right->size(), right);
}

template <class Key, class Value, std::size_t BlockSize, class Less>
typename BPTree<Key, Value, BlockSize, Less>::const_iterator BPTree<Key, Value, BlockSize, Less>::end() const {
    return cend();
}

template <class Key, class Value, std::size_t BlockSize, class Less>
Leaf<Key, Value, BlockSize, Less> *BPTree<Key, Value, BlockSize, Less>::get_left_leaf(
    BPTree::node *curr) const noexcept {
    if (curr == nullptr) {
        return nullptr;
    } else if (curr->is_leaf()) {
        return static_cast<leaf *>(curr);
    }
    return get_left_leaf(static_cast<internal *>(curr)->m_children[0]);
}

template <class Key, class Value, std::size_t BlockSize, class Less>
Leaf<Key, Value, BlockSize, Less> *BPTree<Key, Value, BlockSize, Less>::get_right_leaf(
    BPTree::node *curr) const noexcept {
    if (curr == nullptr) {
        return nullptr;
    } else if (curr->is_leaf()) {
        return static_cast<leaf *>(curr);
    }
    return get_right_leaf(static_cast<internal *>(curr)->m_children[curr->size()]);
}

template <class Key, class Value, std::size_t BlockSize, class Less>
std::size_t BPTree<Key, Value, BlockSize, Less>::get_child_index(BPTree::node *parent,
                                                                 BPTree::node *child) const noexcept {
    for (std::size_t j = 0; j <= parent->size(); j++) {
        if (static_cast<internal *>(parent)->m_children[j] == child) {
            return j;
        }
    }
    return parent->size() + 1;
}

template <class Key, class Value, std::size_t BlockSize, class Less>
Leaf<Key, Value, BlockSize, Less> *BPTree<Key, Value, BlockSize, Less>::get_next_leaf(
    BPTree::leaf *curr_leaf, BPTree::node *curr_root) const noexcept {
    node *parent = find_parent(curr_leaf, curr_root);
    if (parent == nullptr) {
        return nullptr;
    }
    std::size_t i = get_child_index(parent, curr_leaf);

    if (i != parent->size()) {
        return static_cast<leaf *>(static_cast<internal *>(parent)->m_children[i + 1]);
    } else {
        node *curr;
        while (i == parent->size()) {
            if (parent == curr_root) {
                return nullptr;
            }
            curr   = parent;
            parent = find_parent(curr, curr_root);
            i      = get_child_index(parent, curr);
        }
        node *next = static_cast<internal *>(parent)->m_children[i + 1];
        return get_left_leaf(next);
    }
}

template <class Key, class Value, std::size_t BlockSize, class Less>
void BPTree<Key, Value, BlockSize, Less>::set_leaf_connections(node *curr_root) noexcept {
    if (curr_root->is_leaf()) {
        return;
    }
    leaf *curr_leaf = get_left_leaf(curr_root);
    while (curr_leaf != nullptr) {
        leaf *next_leaf   = get_next_leaf(curr_leaf, curr_root);
        curr_leaf->m_next = next_leaf;
        curr_leaf         = next_leaf;
    }
}

template <class Key, class Value, std::size_t BlockSize, class Less>
bool BPTree<Key, Value, BlockSize, Less>::empty() const noexcept {
    return root == nullptr;
}

template <class Key, class Value, std::size_t BlockSize, class Less>
std::size_t BPTree<Key, Value, BlockSize, Less>::size() const noexcept {
    std::size_t cnt = 0;
    leaf *curr      = get_left_leaf(root);
    while (curr != nullptr) {
        cnt += curr->m_count_keys;
        curr = curr->m_next;
    }
    return cnt;
}

template <class Key, class Value, std::size_t BlockSize, class Less>
void BPTree<Key, Value, BlockSize, Less>::clear() {
    node *curr = root;
    root       = nullptr;
    delete curr;
}

template <class Key, class Value, std::size_t BlockSize, class Less>
Leaf<Key, Value, BlockSize, Less> *BPTree<Key, Value, BlockSize, Less>::find_leaf(BPTree::node *start,
                                                                                  const Key &key) const noexcept {
    node *curr = start;
    while (not curr->is_leaf()) {
        std::size_t i = 0;
        while (i < curr->size() && non_strict_compare(static_cast<internal *>(curr)->m_keys[i], key)) {
            i++;
        }
        curr = static_cast<internal *>(curr)->m_children[i];
    }
    return static_cast<leaf *>(curr);
}

template <class Key, class Value, std::size_t BlockSize, class Less>
std::size_t BPTree<Key, Value, BlockSize, Less>::find_index(BPTree::node *curr, const Key &key) const noexcept {
    std::size_t pos = 0;
    while (pos < curr->size() && strict_compare_with_key(curr, pos, key)) {
        pos++;
    }
    return pos;
}

template <class Key, class Value, std::size_t BlockSize, class Less>
Internal<Key, Value, BlockSize, Less> *BPTree<Key, Value, BlockSize, Less>::find_parent(
    BPTree::node *child, BPTree::node *curr_root) const noexcept {
    Key key;
    if (child->is_leaf()) {
        key = static_cast<leaf *>(child)->m_data[0].first;
    } else {
        key = static_cast<internal *>(child)->m_keys[0];
    }

    node *curr   = curr_root;
    node *parent = nullptr;
    while (curr != child) {
        std::size_t i = 0;
        while (i < curr->size() && non_strict_compare(static_cast<internal *>(curr)->m_keys[i], key)) {
            i++;
        }
        parent = curr;
        curr   = static_cast<internal *>(curr)->m_children[i];
    }
    return static_cast<internal *>(parent);
}

template <class Key, class Value, std::size_t BlockSize, class Less>
bool BPTree<Key, Value, BlockSize, Less>::strict_compare_with_key(BPTree::node *curr, std::size_t pos,
                                                                  const Key &key) const noexcept {
    if (curr->is_leaf()) {
        return strict_compare(static_cast<leaf *>(curr)->m_data[pos].first, key);
    } else {
        return strict_compare(static_cast<internal *>(curr)->m_keys[pos], key);
    }
}

template <class Key, class Value, std::size_t BlockSize, class Less>
bool BPTree<Key, Value, BlockSize, Less>::contains(const Key &key) const noexcept {
    if (empty()) {
        return false;
    }
    leaf *curr      = find_leaf(root, key);
    std::size_t pos = find_index(curr, key);
    if (pos != curr->size() && equality(curr->m_data[pos].first, key)) {
        return true;
    }
    return false;
}

template <class Key, class Value, std::size_t BlockSize, class Less>
std::size_t BPTree<Key, Value, BlockSize, Less>::count(const Key &key) const noexcept {
    return contains(key);
}

template <class Key, class Value, std::size_t BlockSize, class Less>
void BPTree<Key, Value, BlockSize, Less>::shift_left(BPTree::internal *curr, std::size_t start, std::size_t end) {
    for (std::size_t i = start; i < end; i++) {
        curr->m_keys[i]     = curr->m_keys[i + 1];
        curr->m_children[i] = curr->m_children[i + 1];
    }
}

template <class Key, class Value, std::size_t BlockSize, class Less>
void BPTree<Key, Value, BlockSize, Less>::shift_left(BPTree::leaf *curr, std::size_t start, std::size_t end) {
    for (std::size_t i = start; i < end; i++) {
        curr->m_data[i].first = curr->m_data[i + 1].first;
        if constexpr (std::is_move_assignable_v<Value>) {
            curr->m_data[i].second = std::move(curr->m_data[i + 1].second);
        } else {
            curr->m_data[i].second = curr->m_data[i + 1].second;
        }
    }
}

template <class Key, class Value, std::size_t BlockSize, class Less>
void BPTree<Key, Value, BlockSize, Less>::shift_right(BPTree::internal *curr, std::size_t start, std::size_t end,
                                                      std::size_t param) {
    for (std::size_t i = start; i >= end; i--) {
        curr->m_keys[i]             = curr->m_keys[i - 1];
        curr->m_children[i + param] = curr->m_children[i - 1 + param];
    }
}

template <class Key, class Value, std::size_t BlockSize, class Less>
void BPTree<Key, Value, BlockSize, Less>::shift_right(BPTree::leaf *curr, std::size_t start, std::size_t end) {
    for (std::size_t i = start; i >= end; i--) {
        curr->m_data[i].first = curr->m_data[i - 1].first;
        if constexpr (std::is_move_assignable_v<Value>) {
            curr->m_data[i].second = std::move(curr->m_data[i - 1].second);
        } else {
            curr->m_data[i].second = curr->m_data[i - 1].second;
        }
    }
}

template <class Key, class Value, std::size_t BlockSize, class Less>
void BPTree<Key, Value, BlockSize, Less>::move_leaf(BPTree::leaf *from, BPTree::leaf *to, std::size_t start,
                                                    std::size_t end, std::size_t i_from, std::size_t i_to) {
    for (std::size_t i = start; i < end; i++) {
        to->m_data[i_to + i].first = from->m_data[i_from + i].first;
        if constexpr (std::is_move_assignable_v<Value>) {
            to->m_data[i_to + i].second = std::move(from->m_data[i_from + i].second);
        } else {
            to->m_data[i_to + i].second = from->m_data[i_from + i].second;
        }
    }
}

template <class Key, class Value, std::size_t BlockSize, class Less>
void BPTree<Key, Value, BlockSize, Less>::move_internal(BPTree::internal *from, BPTree::internal *to,
                                                        std::size_t border) {
    for (std::size_t i = 0; i < from->m_count_keys - border - 1; i++) {
        to->m_keys[i]     = from->m_keys[border + 1 + i];
        to->m_children[i] = from->m_children[border + 1 + i];
    }
}

template <class Key, class Value, std::size_t BlockSize, class Less>
std::pair<std::pair<std::size_t, Leaf<Key, Value, BlockSize, Less> *>, bool>
BPTree<Key, Value, BlockSize, Less>::m_insert(const Key &key, const Value &value) {
    if (empty()) {
        root = new leaf(key, value, nullptr);
        return std::make_pair(std::make_pair(0, static_cast<leaf *>(root)), true);
    }

    leaf *curr      = find_leaf(root, key);
    std::size_t pos = find_index(curr, key);
    if (equality(key, curr->m_data[pos].first)) {
        return std::make_pair(std::make_pair(pos, curr), false);
    }

    shift_right(curr, curr->m_count_keys, pos + 1);
    curr->m_data[pos].first  = key;
    curr->m_data[pos].second = value;
    curr->m_count_keys++;

    return check_leaf(curr, key, pos);
}

template <class Key, class Value, std::size_t BlockSize, class Less>
std::pair<std::pair<std::size_t, Leaf<Key, Value, BlockSize, Less> *>, bool>
BPTree<Key, Value, BlockSize, Less>::m_insert(const Key &key, Value &&value) {
    if (empty()) {
        root = new leaf(key, std::move(value), nullptr);
        return std::make_pair(std::make_pair(0, static_cast<leaf *>(root)), true);
    }

    leaf *curr      = find_leaf(root, key);
    std::size_t pos = find_index(curr, key);
    if (equality(key, curr->m_data[pos].first)) {
        return std::make_pair(std::make_pair(pos, curr), false);
    }

    shift_right(curr, curr->m_count_keys, pos + 1);
    curr->m_data[pos].first  = key;
    curr->m_data[pos].second = std::move(value);
    curr->m_count_keys++;

    return check_leaf(curr, key, pos);
}

template <class Key, class Value, std::size_t BlockSize, class Less>
std::pair<std::pair<std::size_t, Leaf<Key, Value, BlockSize, Less> *>, bool>
BPTree<Key, Value, BlockSize, Less>::check_leaf(BPTree::leaf *curr, const Key &key, std::size_t pos) {
    if (curr->is_full()) {
        split_leaf(curr);
        leaf *another_leaf      = find_leaf(root, key);
        std::size_t another_pos = find_index(another_leaf, key);
        return std::make_pair(std::make_pair(another_pos, another_leaf), true);
    } else {
        return std::make_pair(std::make_pair(pos, curr), true);
    }
}

template <class Key, class Value, std::size_t BlockSize, class Less>
void BPTree<Key, Value, BlockSize, Less>::split_leaf(BPTree::leaf *curr) {
    leaf *new_leaf   = new leaf();
    new_leaf->m_next = curr->m_next;
    curr->m_next     = new_leaf;

    std::size_t border = curr->m_count_keys / 2;
    move_leaf(curr, new_leaf, 0, border, curr->m_count_keys - border, 0);
    curr->m_count_keys -= border;
    new_leaf->m_count_keys = border;
    Key border_key         = new_leaf->m_data[0].first;

    check_internal(curr, new_leaf, border_key);
}

template <class Key, class Value, std::size_t BlockSize, class Less>
void BPTree<Key, Value, BlockSize, Less>::insert_internal(BPTree::internal *curr, const Key &key,
                                                          BPTree::node *new_child) {
    std::size_t pos = find_index(curr, key);
    shift_right(curr, curr->m_count_keys, pos + 1, 1);
    curr->m_keys[pos]         = key;
    curr->m_children[pos + 1] = new_child;
    curr->m_count_keys++;

    if (curr->is_full()) {
        split_internal(curr);
    }
}

template <class Key, class Value, std::size_t BlockSize, class Less>
void BPTree<Key, Value, BlockSize, Less>::check_internal(node *curr, node *new_node, const Key &border_key) {
    if (root == curr) {
        root = new internal(border_key, curr, new_node);
    } else {
        insert_internal(find_parent(curr, root), border_key, new_node);
    }
}

template <class Key, class Value, std::size_t BlockSize, class Less>
void BPTree<Key, Value, BlockSize, Less>::split_internal(BPTree::internal *curr) {
    internal *new_internal = new internal();

    std::size_t border = curr->m_count_keys / 2;
    Key border_key     = curr->m_keys[border];
    move_internal(curr, new_internal, border);
    new_internal->m_count_keys                           = curr->m_count_keys - border - 1;
    new_internal->m_children[new_internal->m_count_keys] = curr->m_children[curr->m_count_keys];
    curr->m_count_keys                                   = border;

    check_internal(curr, new_internal, border_key);
}

template <class Key, class Value, std::size_t BlockSize, class Less>
std::pair<typename BPTree<Key, Value, BlockSize, Less>::iterator, bool> BPTree<Key, Value, BlockSize, Less>::insert(
    const Key &key, const Value &value) {
    auto res = m_insert(key, value);
    iterator it(res.first.first, res.first.second);
    return std::make_pair(it, res.second);
}

template <class Key, class Value, std::size_t BlockSize, class Less>
std::pair<typename BPTree<Key, Value, BlockSize, Less>::iterator, bool> BPTree<Key, Value, BlockSize, Less>::insert(
    const Key &key, Value &&value) {
    auto res = m_insert(key, std::move(value));
    iterator it(res.first.first, res.first.second);
    return std::make_pair(it, res.second);
}

template <class Key, class Value, std::size_t BlockSize, class Less>
std::pair<std::size_t, Leaf<Key, Value, BlockSize, Less> *> BPTree<Key, Value, BlockSize, Less>::m_find(
    const Key &key) const noexcept {
    if (empty()) {
        return std::make_pair(0, nullptr);
    }
    leaf *curr        = find_leaf(root, key);
    std::size_t index = find_index(curr, key);
    if (index < curr->size() && equality(curr->m_data[index].first, key)) {
        return std::make_pair(index, curr);
    }
    leaf *last = get_right_leaf(root);
    return std::make_pair(last->size(), last);
}

template <class Key, class Value, std::size_t BlockSize, class Less>
typename BPTree<Key, Value, BlockSize, Less>::iterator BPTree<Key, Value, BlockSize, Less>::find(const Key &key) {
    auto res = m_find(key);
    return iterator(res.first, res.second);
}

template <class Key, class Value, std::size_t BlockSize, class Less>
typename BPTree<Key, Value, BlockSize, Less>::const_iterator BPTree<Key, Value, BlockSize, Less>::find(
    const Key &key) const {
    auto res = m_find(key);
    return const_iterator(res.first, res.second);
}

template <class Key, class Value, std::size_t BlockSize, class Less>
std::pair<std::size_t, Leaf<Key, Value, BlockSize, Less> *> BPTree<Key, Value, BlockSize, Less>::m_at(
    const Key &key) const {
    if (empty()) {
        throw std::out_of_range("There is no such key");
    }
    leaf *curr        = find_leaf(root, key);
    std::size_t index = find_index(curr, key);
    if (index < curr->size() && equality(key, curr->m_data[index].first)) {
        return std::make_pair(index, curr);
    } else {
        throw std::out_of_range("There is no such key");
    }
}

template <class Key, class Value, std::size_t BlockSize, class Less>
Value &BPTree<Key, Value, BlockSize, Less>::at(const Key &key) {
    auto res = m_at(key);
    return res.second->m_data[res.first].second;
}

template <class Key, class Value, std::size_t BlockSize, class Less>
const Value &BPTree<Key, Value, BlockSize, Less>::at(const Key &key) const {
    auto res = m_at(key);
    return res.second->m_data[res.first].second;
}

template <class Key, class Value, std::size_t BlockSize, class Less>
Value &BPTree<Key, Value, BlockSize, Less>::operator[](const Key &key) {
    return insert(key, Value()).first->second;
}

template <class Key, class Value, std::size_t BlockSize, class Less>
template <class ForwardIt>
void BPTree<Key, Value, BlockSize, Less>::insert(ForwardIt begin, ForwardIt end) {
    for (auto it = begin; it != end; it++) {
        insert(it->first, it->second);
    }
}

template <class Key, class Value, std::size_t BlockSize, class Less>
void BPTree<Key, Value, BlockSize, Less>::insert(std::initializer_list<value_type> list) {
    auto it = list.begin();
    while (it != list.end()) {
        insert(it->first, it->second);
        it++;
    }
}

template <class Key, class Value, std::size_t BlockSize, class Less>
void BPTree<Key, Value, BlockSize, Less>::simple_erase(BPTree::leaf *curr, BPTree::internal *parent, std::size_t pos) {
    shift_left(curr, pos, curr->size() - 1);
    curr->m_count_keys--;
    if (pos == 0 && parent != nullptr) {
        std::size_t index = get_child_index(parent, curr);
        if (index > 0) {
            parent->m_keys[index - 1] = curr->m_data[0].first;
        }
    }
}

template <class Key, class Value, std::size_t BlockSize, class Less>
std::pair<std::pair<std::size_t, Leaf<Key, Value, BlockSize, Less> *>, bool>
BPTree<Key, Value, BlockSize, Less>::m_erase(BPTree::leaf *curr, std::size_t pos, const Key &key) {
    if (not equality(curr->m_data[pos].first, key)) {
        return std::make_pair(std::make_pair(0, nullptr), false);
    }

    if (curr->size() > curr->m_data.size() / 2) {
        internal *parent = find_parent(curr, root);
        simple_erase(curr, parent, pos);
        return std::make_pair(std::make_pair(pos, curr), true);
    } else {
        internal *parent = find_parent(curr, root);
        if (parent == nullptr) {
            if (curr->size() > 0) {
                simple_erase(curr, parent, pos);
                return std::make_pair(std::make_pair(pos, curr), true);
            } else {
                return std::make_pair(std::make_pair(0, curr), false);
            }
        }

        std::size_t index = get_child_index(parent, curr);
        leaf *left        = nullptr;
        leaf *right       = nullptr;
        if (index != 0) {
            left = static_cast<leaf *>(parent->m_children[index - 1]);
        }
        if (index != parent->size()) {
            right = static_cast<leaf *>(parent->m_children[index + 1]);
        }

        if (right != nullptr && (right->size() > right->m_data.size() / 2)) {
            shift_left(curr, pos, curr->size() - 1);
            curr->m_data[curr->m_count_keys - 1].first  = right->m_data[0].first;
            curr->m_data[curr->m_count_keys - 1].second = right->m_data[0].second;
            shift_left(right, 0, right->size() - 1);
            right->m_count_keys--;
            if (index > 1) {
                parent->m_keys[index - 1] = curr->m_data[0].first;
            }
            parent->m_keys[index] = right->m_data[0].first;
        } else if (left != nullptr && (left->size() > left->m_data.size() / 2)) {
            shift_right(curr, pos, 1);
            curr->m_data[0].first  = left->m_data[left->m_count_keys - 1].first;
            curr->m_data[0].second = left->m_data[left->m_count_keys - 1].second;
            left->m_count_keys--;
            parent->m_keys[index - 1] = curr->m_data[0].first;
        } else if (right != nullptr) {
            merge_leaf(curr, right, true, pos);
            erase_internal(parent, index + 1);
            set_leaf_connections(root);
        } else if (left != nullptr) {
            merge_leaf(left, curr, false, pos);
            erase_internal(parent, index);
            set_leaf_connections(root);
        } else {
            simple_erase(curr, parent, pos);
        }
    }

    if (empty()) {
        return std::make_pair(std::make_pair(0, nullptr), true);
    }
    leaf *suspect     = find_leaf(root, key);
    std::size_t index = find_index(suspect, key);
    if (index < suspect->size()) {
        return std::make_pair(std::make_pair(pos, suspect), true);
    } else {
        suspect = suspect->m_next;
        return std::make_pair(std::make_pair(0, suspect), true);
    }
}

template <class Key, class Value, std::size_t BlockSize, class Less>
void BPTree<Key, Value, BlockSize, Less>::merge_leaf(leaf *left, leaf *right, bool left_leaf, std::size_t pos) {
    left->m_next = right->m_next;
    if (left_leaf) {
        shift_left(left, pos, left->size() - 1);
        move_leaf(right, left, 0, right->size(), 0, left->m_count_keys - 1);
    } else {
        move_leaf(right, left, 0, pos, 0, left->size());
        move_leaf(right, left, pos + 1, right->size(), 0, left->size());
    }
    left->m_count_keys += right->m_count_keys - 1;
    delete right;
}

template <class Key, class Value, std::size_t BlockSize, class Less>
void BPTree<Key, Value, BlockSize, Less>::erase_internal(BPTree::internal *curr, std::size_t index) {
    for (std::size_t i = index; i < curr->size(); i++) {
        curr->m_children[i] = curr->m_children[i + 1];
    }
    if (index != 0) {
        for (std::size_t i = index - 1; i < curr->size() - 1; i++) {
            curr->m_keys[i] = curr->m_keys[i + 1];
        }
    }
    curr->m_count_keys--;

    if (root == curr) {
        if (curr->size() == 0) {
            root = copy_tree(curr->m_children[0]);
            set_leaf_connections(root);
            delete curr;
        }
    } else {
        if (curr->m_count_keys < curr->m_keys.size() / 2 && find_parent(curr, root) != nullptr) {
            merge_internal(curr);
        }
    }
}

template <class Key, class Value, std::size_t BlockSize, class Less>
void BPTree<Key, Value, BlockSize, Less>::merge_internal(BPTree::internal *curr) {
    internal *parent  = find_parent(curr, root);
    std::size_t index = get_child_index(parent, curr);
    internal *left    = nullptr;
    internal *right   = nullptr;
    if (index != 0) {
        left = static_cast<internal *>(parent->m_children[index - 1]);
    }
    if (index != parent->size()) {
        right = static_cast<internal *>(parent->m_children[index + 1]);
    }

    if (right != nullptr && right->m_count_keys > right->m_keys.size() / 2) {
        Key border_key                           = right->m_keys[0];
        curr->m_keys[curr->m_count_keys]         = parent->m_keys[index];
        curr->m_children[curr->m_count_keys + 1] = right->m_children[0];
        curr->m_count_keys++;
        shift_left(right, 0, right->size() - 1);
        right->m_children[right->m_count_keys - 1] = right->m_children[right->m_count_keys];

        right->m_count_keys--;
        parent->m_keys[index] = border_key;
    } else if (left != nullptr && left->m_count_keys > left->m_keys.size() / 2) {
        Key border_key                           = left->m_keys[left->m_count_keys - 1];
        curr->m_children[curr->m_count_keys + 1] = curr->m_children[curr->m_count_keys];
        shift_right(curr, curr->size(), 1, 0);
        curr->m_count_keys++;
        curr->m_keys[0]     = parent->m_keys[index - 1];
        curr->m_children[0] = left->m_children[left->m_count_keys];

        left->m_count_keys--;
        parent->m_keys[index - 1] = border_key;
    } else if (left != nullptr && (left->m_count_keys + curr->m_count_keys < curr->m_keys.size())) {
        leaf *left_leaf                  = get_left_leaf(curr);
        Key border_key                   = left_leaf->m_data[0].first;
        left->m_keys[left->m_count_keys] = border_key;

        for (std::size_t i = 0; i < curr->m_count_keys; i++) {
            left->m_count_keys++;
            left->m_keys[left->m_count_keys]     = curr->m_keys[i];
            left->m_children[left->m_count_keys] = copy_tree(curr->m_children[i]);
        }
        left->m_count_keys++;
        left->m_children[left->m_count_keys] = copy_tree(curr->m_children[curr->m_count_keys]);

        erase_internal(parent, index);
        delete curr;
    } else if (right != nullptr && (right->m_count_keys + curr->m_count_keys < curr->m_keys.size())) {
        leaf *left_leaf                  = get_left_leaf(right);
        Key border_key                   = left_leaf->m_data[0].first;
        curr->m_keys[curr->m_count_keys] = border_key;

        for (std::size_t i = 0; i < right->m_count_keys; i++) {
            curr->m_count_keys++;
            curr->m_keys[curr->m_count_keys]     = right->m_keys[i];
            curr->m_children[curr->m_count_keys] = copy_tree(right->m_children[i]);
        }
        curr->m_count_keys++;
        curr->m_children[curr->m_count_keys] = copy_tree(right->m_children[right->m_count_keys]);

        erase_internal(parent, index + 1);
        delete right;
    }
}

template <class Key, class Value, std::size_t BlockSize, class Less>
std::size_t BPTree<Key, Value, BlockSize, Less>::erase(const Key &key) {
    if (empty()) {
        return 0;
    }
    leaf *curr        = find_leaf(root, key);
    std::size_t index = find_index(curr, key);
    auto res          = m_erase(curr, index, key);
    return res.second;
}

template <class Key, class Value, std::size_t BlockSize, class Less>
typename BPTree<Key, Value, BlockSize, Less>::iterator BPTree<Key, Value, BlockSize, Less>::erase(
    BPTree::const_iterator it) {
    Key key           = it->first;
    leaf *curr        = find_leaf(root, key);
    std::size_t index = find_index(curr, key);
    auto res          = m_erase(curr, index, key);
    return iterator(res.first.first, res.first.second);
}

template <class Key, class Value, std::size_t BlockSize, class Less>
typename BPTree<Key, Value, BlockSize, Less>::iterator BPTree<Key, Value, BlockSize, Less>::erase(
    BPTree::const_iterator start, BPTree::const_iterator end) {
    std::pair<std::pair<std::size_t, leaf *>, bool> res;
    std::vector<Key> erase_keys;
    for (auto it = start; it != end; ++it) {
        erase_keys.emplace_back(it->first);
    }

    for (std::size_t i = 0; i < erase_keys.size(); i++) {
        Key key = erase_keys[i];
        if (empty()) {
            return iterator(0, nullptr);
        }
        leaf *curr        = find_leaf(root, key);
        std::size_t index = find_index(curr, key);
        res               = m_erase(curr, index, key);
    }
    return iterator(res.first.first, res.first.second);
}

template <class Key, class Value, std::size_t BlockSize, class Less>
std::pair<std::size_t, Leaf<Key, Value, BlockSize, Less> *> BPTree<Key, Value, BlockSize, Less>::m_lower_bound(
    const Key &key) const noexcept {
    leaf *curr        = find_leaf(root, key);
    std::size_t index = find_index(curr, key);
    if (index < curr->size()) {
        return std::make_pair(index, curr);
    } else {
        leaf *right = get_right_leaf(root);
        return std::make_pair(right->size(), right);
    }
}
template <class Key, class Value, std::size_t BlockSize, class Less>
typename BPTree<Key, Value, BlockSize, Less>::iterator BPTree<Key, Value, BlockSize, Less>::lower_bound(
    const Key &key) {
    if (empty()) {
        return end();
    }
    auto res = m_lower_bound(key);
    return iterator(res.first, res.second);
}

template <class Key, class Value, std::size_t BlockSize, class Less>
typename BPTree<Key, Value, BlockSize, Less>::const_iterator BPTree<Key, Value, BlockSize, Less>::lower_bound(
    const Key &key) const {
    if (empty()) {
        return end();
    }
    auto res = m_lower_bound(key);
    return const_iterator(res.first, res.second);
}

template <class Key, class Value, std::size_t BlockSize, class Less>
std::pair<std::size_t, Leaf<Key, Value, BlockSize, Less> *> BPTree<Key, Value, BlockSize, Less>::m_upper_bound(
    const Key &key) const noexcept {
    leaf *curr        = find_leaf(root, key);
    std::size_t index = find_index(curr, key);
    if (index < curr->size() - 1 && equality(curr->m_data[index].first, key)) {
        return std::make_pair(index + 1, curr);
    } else if (index == curr->size() - 1 && equality(curr->m_data[index].first, key)) {
        leaf *next = curr->m_next;
        if (next == nullptr) {
            return std::make_pair(curr->size(), curr);
        }
        return std::make_pair(0, next);
    } else if (index < curr->size()) {
        return std::make_pair(index, curr);
    } else {
        leaf *right = get_right_leaf(root);
        return std::make_pair(right->size(), right);
    }
}

template <class Key, class Value, std::size_t BlockSize, class Less>
typename BPTree<Key, Value, BlockSize, Less>::iterator BPTree<Key, Value, BlockSize, Less>::upper_bound(
    const Key &key) {
    if (empty()) {
        return iterator(0, nullptr);
    }
    auto res = m_upper_bound(key);
    return iterator(res.first, res.second);
}

template <class Key, class Value, std::size_t BlockSize, class Less>
typename BPTree<Key, Value, BlockSize, Less>::const_iterator BPTree<Key, Value, BlockSize, Less>::upper_bound(
    const Key &key) const {
    if (empty()) {
        return const_iterator(0, nullptr);
    }
    auto res = m_upper_bound(key);
    return const_iterator(res.first, res.second);
}

template <class Key, class Value, std::size_t BlockSize, class Less>
std::pair<typename BPTree<Key, Value, BlockSize, Less>::iterator,
          typename BPTree<Key, Value, BlockSize, Less>::iterator>
BPTree<Key, Value, BlockSize, Less>::equal_range(const Key &key) {
    iterator left  = lower_bound(key);
    iterator right = upper_bound(key);
    return std::make_pair(left, right);
}

template <class Key, class Value, std::size_t BlockSize, class Less>
std::pair<typename BPTree<Key, Value, BlockSize, Less>::const_iterator,
          typename BPTree<Key, Value, BlockSize, Less>::const_iterator>
BPTree<Key, Value, BlockSize, Less>::equal_range(const Key &key) const {
    const_iterator left  = lower_bound(key);
    const_iterator right = upper_bound(key);
    return std::make_pair(left, right);
}

#endif