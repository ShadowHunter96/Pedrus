package cz.bbn.cerberus.contactpersontype.persistence;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface ContactPersonTypeRepository extends JpaRepository<ContactPersonTypeEntity, String> {

    @Query(" select entity " +
            "from ContactPersonTypeEntity entity " +
            "where entity.allowed = true and entity.id != :exceptId")
    List<ContactPersonTypeEntity> findAllEnabled(String exceptId);

    @Query(" select entity " +
            "from ContactPersonTypeEntity entity " +
            "where entity.allowed = true")
    List<ContactPersonTypeEntity> findAllEnabled();

    @Query("select entity.allowed " +
            "from ContactPersonTypeEntity entity " +
            "where entity.id = :id")
    boolean isContactPersonTypeAllowed(String id);
}
