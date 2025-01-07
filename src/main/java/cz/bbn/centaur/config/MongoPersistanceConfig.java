package cz.bbn.cerberus.config;

import org.springframework.context.annotation.Configuration;
import org.springframework.data.mongodb.repository.config.EnableMongoRepositories;


@Configuration
@EnableMongoRepositories(basePackages = "cz.solutia.cerberus")
public class MongoPersistanceConfig {

}
