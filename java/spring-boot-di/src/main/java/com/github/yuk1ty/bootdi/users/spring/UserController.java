package com.github.yuk1ty.bootdi.users.spring;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/users")
public class UserController {

    // もうここには DI コンテナで解決済みのインスタンスが来ている。
    private UserService userService;
}
