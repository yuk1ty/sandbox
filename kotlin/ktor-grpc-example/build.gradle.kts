plugins {
    alias(libs.plugins.kotlin.jvm)
    alias(libs.plugins.kotlin.serialization)
    alias(libs.plugins.google.ksp)
}

group = "com.github.yuk1ty"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    implementation(libs.bundles.ktor)
    implementation(libs.bundles.serialization)
    implementation(libs.bundles.koin)
    ksp(libs.koin.compiler)
    testImplementation(kotlin("test"))
}

tasks.test {
    useJUnitPlatform()
}
kotlin {
    jvmToolchain(21)
}

sourceSets.main {
    java.srcDirs("build/generated/ksp/main/kotlin")
}

ksp {
    // Turns on compile-time dependency checking
    arg("KOTLIN_CONFIG_CHECK", "true")
}