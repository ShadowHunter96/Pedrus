package cz.bbn.cerberus.config;

import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.enviromennt.AppProperty;
import lombok.extern.slf4j.Slf4j;
import org.apache.hadoop.fs.FileSystem;
import org.springframework.context.annotation.Bean;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

//@Configuration
@Slf4j
public class HadoopConfig {

    private final AppEnv appEnv;

    public HadoopConfig(AppEnv appEnv) {
        this.appEnv = appEnv;
    }

    @Bean
    public org.apache.hadoop.conf.Configuration getConfiguration() {
        org.apache.hadoop.conf.Configuration configuration = new org.apache.hadoop.conf.Configuration();
        configuration.set("fs.defaultFS", appEnv.getProperty(AppProperty.HADOOP_PATH));
        return configuration;
    }

    @Bean
    public FileSystem getFileSystem() {
        FileSystem fileSystem = null;
        try {
            fileSystem = FileSystem.get(
                    new URI(appEnv.getProperty(AppProperty.HADOOP_PATH)), getConfiguration(),
                    appEnv.getProperty(AppProperty.HADOOP_USER));
        } catch (IOException | InterruptedException | URISyntaxException e) {
            log.error(e.getMessage());
            Thread.currentThread().interrupt();
        }
        return fileSystem;
    }
}
