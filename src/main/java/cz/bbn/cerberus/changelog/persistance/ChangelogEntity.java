package cz.bbn.cerberus.changelog.persistance;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import java.time.LocalDateTime;

@Getter
@Setter
@NoArgsConstructor
@Entity
@Table(name = "changelog", schema = "other")
public class ChangelogEntity {

    @Id
    private String id;

    @Column(name = "create_date")
    private LocalDateTime createDate;

    @Column(name = "release_date")
    private LocalDateTime releaseDate;

    private String version;

    private String text;

    public ChangelogEntity(String version) {
        this.version = version;
    }
}
