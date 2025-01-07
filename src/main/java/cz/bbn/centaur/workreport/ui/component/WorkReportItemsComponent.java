package cz.bbn.cerberus.workreport.ui.component;

import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.H3;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import cz.bbn.cerberus.phase.dto.PhaseDto;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.workreport.dto.ProjectPhaseActivityDto;
import cz.bbn.cerberus.workreport.dto.WorkReportPickDto;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;

public class WorkReportItemsComponent extends VerticalLayout {

    private final WorkReportListener listener;
    private final List<WorkReportPickDto> pickDtoList;
    private final Map<String, List<PhaseDto>> projectPhaseMap;


    public WorkReportItemsComponent(WorkReportListener listener, ProjectPhaseActivityDto projectPhaseActivityDto) {
        this.pickDtoList = projectPhaseActivityDto.getPickItemList();
        this.projectPhaseMap = projectPhaseActivityDto.getProjectPhaseMap();
        this.listener = listener;
        initComponent();
    }

    private void initComponent() {
        this.setMargin(false);
        this.setPadding(false);
        Div cardDiv = getCardDiv();
        this.add(cardDiv);
    }


    private Div getCardDiv() {
        Div cardDiv = new Div();
        cardDiv.setWidth("16em");
        cardDiv.setHeightFull();
        this.getElement().getStyle().set("padding", "1em").set("padding-right", "0px").set("padding-top", "2em");
        this.setWidth("17em");
        this.setHeightFull();
        cardDiv.getElement().getStyle().set("border-radius", "1em")
                .set("background-color", "var(--lumo-primary-contrast-color-10pct)");

        H3 title = new H3(Transl.get("Projects").concat("/").concat(Transl.get("Phases")));
        title.getElement().getStyle().set("text-align", "center").set("font-weight", "700");
        cardDiv.add(title);

        Span line = new Span();
        line.getElement().getStyle().set("background-color", "var(--lumo-primary-color)").set("display", "block");
        line.setWidthFull();
        line.setHeight("2px");
        cardDiv.add(line);

        Div contentDiv = new Div();
        contentDiv.setWidthFull();
        contentDiv.setHeight("calc(100% - 5em)");
        contentDiv.getElement().getStyle().set("overflow", "auto");
        cardDiv.add(contentDiv);

        addProjectsPhases(contentDiv);

        return cardDiv;
    }

    private void addProjectsPhases(Div cardDiv) {
        pickDtoList.sort(Comparator.comparing(WorkReportPickDto::getName));
        for (WorkReportPickDto pickDto : pickDtoList) {
            Div project = new Div();
            project.getElement().getStyle().set("padding-left", "20px").set("padding-top", "15px")
                    .set("font-weight", "750");
            project.addClassName("cursor-pointer");
            project.add(pickDto.getName());
            project.addClickListener(e -> listener.changeProjectAndPhase(pickDto, null));
            cardDiv.add(project);

            List<PhaseDto> phaseList = new ArrayList<>(projectPhaseMap.get(pickDto.getId()));
            phaseList.sort(Comparator.comparing(PhaseDto::getName));
            for (PhaseDto phaseDto : phaseList) {
                Div phase = new Div();
                phase.getElement().getStyle().set("padding-left", "30px").set("padding-top", "10px")
                        .set("font-weight", "750");
                phase.addClassName("cursor-pointer");
                phase.add(phaseDto.getName());
                phase.addClickListener(e -> listener.changeProjectAndPhase(pickDto, phaseDto));
                cardDiv.add(phase);
            }
        }
    }
}
