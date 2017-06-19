class RabinHash(object):
    HASH_P = 2000000089  # prime number
    HASH_C = 1112064  # input alphabet size

    def __init__(self, text, window_size):
        self.text = text
        self.window_size = window_size

    def __iter__(self):
        h = 0
        pos = 0
        need = self.window_size
        while need and pos < len(self.text):
            if self.text[pos].isalpha():
                need -= 1
                h = (ord(self.text[pos].lower()) + h * self.HASH_C) % self.HASH_P
            pos += 1
        if need > 0:
            # not enough chars to form first window
            return
        yield h, pos

        top_c = 1
        for _ in range(self.window_size - 1):
            top_c = top_c * self.HASH_C % self.HASH_P

        start_pos = 0
        for i in range(pos, len(self.text)):
            if not self.text[i].isalpha():
                continue
            while not self.text[start_pos].isalpha():
                start_pos += 1
            h = h - ord(self.text[start_pos].lower()) * top_c
            h = (h * self.HASH_C + ord(self.text[i].lower())) % self.HASH_P
            start_pos += 1
            yield h, start_pos

# rhash = RabinHash(", ! ab, abb,,,", 2)
# for h in rhash:
#     print h
