package com.github.yuk1ty.bootdi.users.spring;

import com.github.yuk1ty.bootdi.users.User;
import com.github.yuk1ty.bootdi.users.spring.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class UserService {

    private final UserRepository userRepository;

    @Autowired
    public UserService(UserRepository userRepository) {
        this.userRepository = userRepository;
    }

    public void deactivate(String userId) {
        var user = this.userRepository.findUser(userId);
        user.ifPresent(u -> {
            var updated = new User(u.userId(), u.userName(), false);
            this.userRepository.updateUser(updated);
        });
    }
}
