package cz.bbn.cerberus.changelog.persistance;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface ChangelogRepository extends JpaRepository<ChangelogEntity, String> {

    @Query("select new ChangelogEntity(changelogEntity.version) " +
            "from ChangelogEntity changelogEntity " +
            "order by changelogEntity.releaseDate desc")
    List<ChangelogEntity> findAllVersionList();

    Optional<ChangelogEntity> findChangelogByVersion(String version);
}
