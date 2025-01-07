package cz.bbn.cerberus.changelog.factory;

import cz.bbn.cerberus.changelog.dto.ChangelogVersionDto;
import cz.bbn.cerberus.changelog.persistance.ChangelogEntity;

public class ChangelogFactory {

    private ChangelogFactory() {
    }

    public static ChangelogVersionDto fromEntityToVersionDto(ChangelogEntity changelogEntity){
        return new ChangelogVersionDto(changelogEntity.getVersion());
    }
}
