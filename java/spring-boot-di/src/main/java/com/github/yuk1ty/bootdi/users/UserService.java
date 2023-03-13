package com.github.yuk1ty.bootdi.users;

public class UserService {

    private final UserRepository userRepository;

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
