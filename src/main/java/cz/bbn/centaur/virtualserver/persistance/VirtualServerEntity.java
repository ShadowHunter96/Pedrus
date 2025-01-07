package cz.bbn.cerberus.virtualserver.persistance;

import cz.bbn.cerberus.enumeration.persistance.entity.EnumerationEntity;
import cz.bbn.cerberus.user.persistance.UserEntity;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Table(name = "virtual_server", schema = "intranet")
@Getter
@Setter
public class VirtualServerEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String name;
    private String os;
    private Integer cpu;
    private Integer cores;
    private Integer ram;
    private String ip;

    @OneToOne
    @JoinColumn(name = "user_id", referencedColumnName = "id")
    private UserEntity owner;

    @Column(name = "creation_date")
    private LocalDateTime creationDate;

    @Column(name = "request_date")
    private LocalDateTime requestDate;

    private String status;
    private Boolean deleted;

    @OneToMany(fetch = FetchType.EAGER)
    @JoinColumn(name = "virtual_server_id", referencedColumnName = "id")
    private List<HddEntity> hddEntityList;

    @OneToOne
    @JoinColumn(name = "subnet", referencedColumnName = "id")
    private EnumerationEntity subnet;

    @Column(name = "string_id")
    private String stringId;

    @Column(name = "notification_period")
    private String notificationPeriod;
}
