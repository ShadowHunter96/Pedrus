package cz.bbn.cerberus.commons.component.ui.appcard;

import com.github.appreciated.apexcharts.ApexCharts;
import com.github.appreciated.apexcharts.ApexChartsBuilder;
import com.github.appreciated.apexcharts.config.builder.ChartBuilder;
import com.github.appreciated.apexcharts.config.builder.LegendBuilder;
import com.github.appreciated.apexcharts.config.chart.Type;
import com.github.appreciated.apexcharts.config.legend.Position;
import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.commons.component.ui.interfaces.MapItemDtoAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.RouteAction;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

public class AppPieGraphCard extends AppDashboardCard {

    private final MapItemDtoAction<ItemDto, Double> mapItemDtoAction;

    public AppPieGraphCard(String title, MapItemDtoAction<ItemDto, Double> mapItemDtoAction, String route) {
        super(title, route);
        this.mapItemDtoAction = mapItemDtoAction;
        reloadData();
    }

    public AppPieGraphCard(String title, MapItemDtoAction<ItemDto, Double> mapItemDtoAction, RouteAction routeAction) {
        super(title, routeAction);
        this.mapItemDtoAction = mapItemDtoAction;
        reloadData();
    }

    @Override
    public void reloadData() {
        this.getContent().setHeight("12em");
        this.setWidth("25em");
        getContent().removeAll();
        AtomicReference<Double> count = new AtomicReference<>((double) 0);
        Map<ItemDto, Double> map = mapItemDtoAction.getMap();
        List<Double> valueList = new ArrayList<>();
        List<String> labelList = new ArrayList<>();
        map.forEach((s, value) -> {
            valueList.add(value);
            labelList.add(s.getName() + " - " + value.intValue());
            count.updateAndGet(v ->  v + value);
        });
        setHeader(getTitle() + " - " + count.get().intValue());

        ApexCharts donutChart = ApexChartsBuilder.get()
                .withChart(ChartBuilder.get().withType(Type.PIE).build())
                .withLegend(LegendBuilder.get()
                        .withPosition(Position.RIGHT)
                        .withWidth(145D)
                        .build())
                .withSeries(valueList.toArray(new Double[valueList.size()]))
                .withLabels(labelList.toArray(new String[labelList.size()]))
                .build();
        getContent().add(donutChart);
    }
}
