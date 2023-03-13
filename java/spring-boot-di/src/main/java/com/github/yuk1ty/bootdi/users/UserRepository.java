package com.github.yuk1ty.bootdi.users;

import java.util.Optional;

public interface UserRepository {

    Optional<User> findUser(String userId);

    void updateUser(User user);
}
