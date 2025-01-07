package cz.bbn.cerberus.changelog;


import cz.bbn.cerberus.changelog.dto.ChangelogVersionDto;
import cz.bbn.cerberus.changelog.factory.ChangelogFactory;
import cz.bbn.cerberus.changelog.persistance.ChangelogEntity;
import cz.bbn.cerberus.changelog.persistance.ChangelogRepository;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class ChangelogService {

    private final ChangelogRepository changelogRepository;

    public ChangelogService(ChangelogRepository changelogRepository) {
        this.changelogRepository = changelogRepository;
    }

    public List<ChangelogVersionDto> findAllVersionList() {
        List<ChangelogEntity> changelogEntityList = changelogRepository.findAllVersionList();
        return ConvertEntities.fromEntities(changelogEntityList, ChangelogFactory::fromEntityToVersionDto);
    }

    public ChangelogEntity getChangelog(String version) {
        return Optional.of(changelogRepository.findChangelogByVersion(version)).get().orElse(new ChangelogEntity());
    }

}
