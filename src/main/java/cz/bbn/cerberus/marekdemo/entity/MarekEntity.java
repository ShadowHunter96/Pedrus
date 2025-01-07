package cz.bbn.cerberus.marekdemo.entity;

import lombok.*;

import javax.persistence.*;

/**
 * Created by marek.vu on 04.10.2023.
 */
@Entity
@Getter
@Setter
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table(name = "marek_entity")
public class MarekEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;
    @Column(name = "name")
    private String name;
    @Column(name = "description")
    private String description;
}
