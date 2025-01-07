package cz.bbn.cerberus.virtualserver.persistance;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "vs_hdd", schema = "intranet")
@Getter
@Setter
public class HddEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    private String name;
    private Integer size;

    @Column(name = "virtual_server_id")
    private Long virtualServerId;
}
