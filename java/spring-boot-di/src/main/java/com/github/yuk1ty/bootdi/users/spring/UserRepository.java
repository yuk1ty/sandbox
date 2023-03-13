package com.github.yuk1ty.bootdi.users.spring;

import com.github.yuk1ty.bootdi.users.User;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
public class UserRepository {

    public UserRepository() {
        // TODO
    }

    public Optional<User> findUser(String userId) {
        return Optional.empty();
    }

    public void updateUser(User user) {
        // update
    }
}
