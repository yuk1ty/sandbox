package com.github.yuk1ty.bootdi.users;

import java.util.Optional;

public class UserRepositoryOnMemory implements UserRepository {

    public UserRepositoryOnMemory() {
        // TODO
    }

    public Optional<User> findUser(String userId) {
        return Optional.empty();
    }

    public void updateUser(User user) {
        // update
    }
}
