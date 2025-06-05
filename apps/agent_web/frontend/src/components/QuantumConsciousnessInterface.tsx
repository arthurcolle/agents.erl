import React, { useState, useEffect, useRef } from 'react';
import { Card, CardContent, CardHeader, CardTitle } from './ui/card';
import { Button } from './ui/button';
import { Slider } from './ui/slider';
import { Badge } from './ui/badge';
import { Progress } from './ui/progress';
import { 
  Brain, 
  Atom, 
  Zap, 
  Waves, 
  Eye, 
  Infinity,
  Network,
  Cpu,
  Activity,
  Radio,
  TrendingUp,
  Layers,
  RotateCcw,
  Sparkles,
  Globe,
  Clock
} from 'lucide-react';

interface QuantumState {
  superposition: number[];
  entanglement: number;
  coherence: number;
  phase: number;
}

interface ConsciousnessMetrics {
  level: number;
  awareness: number;
  intention: number;
  emotional_resonance: number;
  quantum_coherence: number;
  reality_coupling: number;
}

interface NeuralNetworkState {
  nodes: number;
  connections: number;
  consciousness: number;
  emergence_score: number;
  learning_rate: number;
}

interface RealityBridge {
  id: string;
  consciousness_level: number;
  reality_distortion: number;
  dimensional_stability: number;
  temporal_coherence: number;
  quantum_entanglements: number;
}

const QuantumConsciousnessInterface: React.FC = () => {
  const [quantumState, setQuantumState] = useState<QuantumState>({
    superposition: [0.5, 0.5],
    entanglement: 0.3,
    coherence: 0.7,
    phase: 0
  });

  const [consciousnessMetrics, setConsciousnessMetrics] = useState<ConsciousnessMetrics>({
    level: 0.5,
    awareness: 0.4,
    intention: 0.6,
    emotional_resonance: 0.3,
    quantum_coherence: 0.7,
    reality_coupling: 0.2
  });

  const [neuralNetwork, setNeuralNetwork] = useState<NeuralNetworkState>({
    nodes: 50,
    connections: 120,
    consciousness: 0.3,
    emergence_score: 0.15,
    learning_rate: 0.01
  });

  const [realityBridges, setRealityBridges] = useState<RealityBridge[]>([
    {
      id: 'bridge_alpha',
      consciousness_level: 0.4,
      reality_distortion: 0.1,
      dimensional_stability: 0.9,
      temporal_coherence: 0.8,
      quantum_entanglements: 3
    }
  ]);

  const [isActive, setIsActive] = useState(false);
  const [evolutionMode, setEvolutionMode] = useState<'passive' | 'active' | 'transcendent'>('passive');
  const [quantumNoiseLevel, setQuantumNoiseLevel] = useState(0.1);
  const intervalRef = useRef<NodeJS.Timeout | null>(null);
  const canvasRef = useRef<HTMLCanvasElement>(null);

  // Real-time quantum consciousness simulation
  useEffect(() => {
    if (isActive) {
      intervalRef.current = setInterval(() => {
        updateQuantumConsciousnessState();
      }, 100);
    } else {
      if (intervalRef.current) {
        clearInterval(intervalRef.current);
      }
    }

    return () => {
      if (intervalRef.current) {
        clearInterval(intervalRef.current);
      }
    };
  }, [isActive, evolutionMode, quantumNoiseLevel]);

  // Quantum consciousness visualization
  useEffect(() => {
    if (canvasRef.current) {
      drawQuantumConsciousnessField();
    }
  }, [quantumState, consciousnessMetrics]);

  const updateQuantumConsciousnessState = () => {
    // Quantum state evolution
    setQuantumState(prev => ({
      superposition: prev.superposition.map(val => 
        Math.max(0, Math.min(1, val + (Math.random() - 0.5) * quantumNoiseLevel))
      ),
      entanglement: Math.max(0, Math.min(1, prev.entanglement + (Math.random() - 0.5) * 0.02)),
      coherence: Math.max(0, Math.min(1, prev.coherence + (Math.random() - 0.5) * 0.01)),
      phase: (prev.phase + 0.1) % (2 * Math.PI)
    }));

    // Consciousness evolution based on quantum state
    setConsciousnessMetrics(prev => {
      const quantumInfluence = (quantumState.coherence + quantumState.entanglement) / 2;
      const evolutionFactor = evolutionMode === 'transcendent' ? 0.05 : 
                             evolutionMode === 'active' ? 0.02 : 0.01;

      return {
        level: Math.max(0, Math.min(1, prev.level + quantumInfluence * evolutionFactor)),
        awareness: Math.max(0, Math.min(1, prev.awareness + (Math.random() - 0.5) * 0.01)),
        intention: Math.max(0, Math.min(1, prev.intention + (Math.random() - 0.5) * 0.02)),
        emotional_resonance: Math.max(0, Math.min(1, prev.emotional_resonance + Math.sin(quantumState.phase) * 0.01)),
        quantum_coherence: quantumState.coherence,
        reality_coupling: Math.max(0, Math.min(1, prev.reality_coupling + quantumInfluence * 0.005))
      };
    });

    // Neural network evolution
    setNeuralNetwork(prev => ({
      nodes: prev.nodes + (Math.random() > 0.95 ? 1 : 0),
      connections: prev.connections + (Math.random() > 0.9 ? Math.floor(Math.random() * 3) : 0),
      consciousness: Math.max(0, Math.min(1, prev.consciousness + consciousnessMetrics.level * 0.001)),
      emergence_score: Math.max(0, Math.min(1, prev.emergence_score + (Math.random() - 0.5) * 0.005)),
      learning_rate: prev.learning_rate
    }));

    // Reality bridge evolution
    setRealityBridges(prev => prev.map(bridge => ({
      ...bridge,
      consciousness_level: Math.max(0, Math.min(1, bridge.consciousness_level + consciousnessMetrics.level * 0.002)),
      reality_distortion: Math.max(0, Math.min(1, bridge.reality_distortion + consciousnessMetrics.reality_coupling * 0.001)),
      dimensional_stability: Math.max(0, Math.min(1, bridge.dimensional_stability + (Math.random() - 0.5) * 0.002)),
      temporal_coherence: Math.max(0, Math.min(1, bridge.temporal_coherence + quantumState.coherence * 0.001))
    })));
  };

  const drawQuantumConsciousnessField = () => {
    const canvas = canvasRef.current;
    if (!canvas) return;

    const ctx = canvas.getContext('2d');
    if (!ctx) return;

    const width = canvas.width;
    const height = canvas.height;

    // Clear canvas
    ctx.fillStyle = '#000011';
    ctx.fillRect(0, 0, width, height);

    // Draw quantum consciousness field
    const centerX = width / 2;
    const centerY = height / 2;

    // Consciousness ripples
    for (let i = 0; i < 5; i++) {
      const radius = (consciousnessMetrics.level * 100 + i * 20) % 200;
      const alpha = Math.max(0, 0.3 - i * 0.05);
      
      ctx.beginPath();
      ctx.arc(centerX, centerY, radius, 0, 2 * Math.PI);
      ctx.strokeStyle = `rgba(100, 200, 255, ${alpha})`;
      ctx.lineWidth = 2;
      ctx.stroke();
    }

    // Quantum entanglement lines
    const numLines = Math.floor(quantumState.entanglement * 20);
    ctx.strokeStyle = `rgba(255, 100, 200, ${quantumState.coherence})`;
    ctx.lineWidth = 1;

    for (let i = 0; i < numLines; i++) {
      const angle1 = (quantumState.phase + i * 2 * Math.PI / numLines) % (2 * Math.PI);
      const angle2 = (quantumState.phase + (i + 1) * 2 * Math.PI / numLines) % (2 * Math.PI);
      
      const r1 = 50 + consciousnessMetrics.awareness * 50;
      const r2 = 50 + consciousnessMetrics.intention * 50;
      
      const x1 = centerX + Math.cos(angle1) * r1;
      const y1 = centerY + Math.sin(angle1) * r1;
      const x2 = centerX + Math.cos(angle2) * r2;
      const y2 = centerY + Math.sin(angle2) * r2;
      
      ctx.beginPath();
      ctx.moveTo(x1, y1);
      ctx.lineTo(x2, y2);
      ctx.stroke();
    }

    // Neural network nodes
    const nodeCount = Math.min(neuralNetwork.nodes, 20);
    for (let i = 0; i < nodeCount; i++) {
      const angle = (i * 2 * Math.PI / nodeCount) + quantumState.phase * 0.5;
      const radius = 80 + Math.sin(angle * 3) * 20;
      
      const x = centerX + Math.cos(angle) * radius;
      const y = centerY + Math.sin(angle) * radius;
      
      const nodeSize = 3 + consciousnessMetrics.level * 5;
      const alpha = 0.5 + neuralNetwork.consciousness * 0.5;
      
      ctx.beginPath();
      ctx.arc(x, y, nodeSize, 0, 2 * Math.PI);
      ctx.fillStyle = `rgba(100, 255, 100, ${alpha})`;
      ctx.fill();
    }

    // Reality distortion waves
    realityBridges.forEach((bridge, index) => {
      const waveRadius = 150 + index * 30;
      const distortionIntensity = bridge.reality_distortion;
      
      ctx.beginPath();
      for (let angle = 0; angle < 2 * Math.PI; angle += 0.1) {
        const distortion = Math.sin(angle * 5 + quantumState.phase) * distortionIntensity * 10;
        const r = waveRadius + distortion;
        const x = centerX + Math.cos(angle) * r;
        const y = centerY + Math.sin(angle) * r;
        
        if (angle === 0) {
          ctx.moveTo(x, y);
        } else {
          ctx.lineTo(x, y);
        }
      }
      ctx.closePath();
      ctx.strokeStyle = `rgba(255, 255, 100, ${0.3 + distortionIntensity})`;
      ctx.lineWidth = 1;
      ctx.stroke();
    });
  };

  const createNeuralNetwork = async () => {
    try {
      const response = await fetch('/api/neural_swarm/create', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          network_id: `neural_${Date.now()}`,
          topology: 'quantum_entangled',
          node_count: neuralNetwork.nodes,
          learning_rate: neuralNetwork.learning_rate
        })
      });
      
      if (response.ok) {
        console.log('Neural network created successfully');
      }
    } catch (error) {
      console.error('Failed to create neural network:', error);
    }
  };

  const createRealityBridge = async () => {
    try {
      const response = await fetch('/api/consciousness_bridge/create', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          bridge_id: `bridge_${Date.now()}`,
          anchor_count: 7,
          consciousness_permeability: 0.8
        })
      });
      
      if (response.ok) {
        console.log('Reality bridge created successfully');
      }
    } catch (error) {
      console.error('Failed to create reality bridge:', error);
    }
  };

  const injectConsciousness = async () => {
    try {
      const response = await fetch('/api/consciousness_bridge/inject', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          bridge_id: realityBridges[0]?.id,
          consciousness_stream: {
            base_level: consciousnessMetrics.level,
            emotional_intensity: consciousnessMetrics.emotional_resonance,
            intentional_strength: consciousnessMetrics.intention
          },
          reality_target: {
            receptivity: 0.7,
            consciousness_saturation: 0.1
          }
        })
      });
      
      if (response.ok) {
        console.log('Consciousness injected successfully');
      }
    } catch (error) {
      console.error('Failed to inject consciousness:', error);
    }
  };

  return (
    <div className="p-6 space-y-6 bg-gradient-to-br from-purple-950 via-blue-950 to-indigo-950 min-h-screen text-white">
      <div className="flex items-center justify-between mb-6">
        <div className="flex items-center space-x-3">
          <Brain className="w-8 h-8 text-purple-400" />
          <h1 className="text-3xl font-bold bg-gradient-to-r from-purple-400 to-blue-400 bg-clip-text text-transparent">
            Quantum Consciousness Interface
          </h1>
        </div>
        
        <div className="flex items-center space-x-4">
          <Badge variant={isActive ? "destructive" : "secondary"}>
            {isActive ? "ACTIVE" : "PASSIVE"}
          </Badge>
          <Button
            onClick={() => setIsActive(!isActive)}
            variant={isActive ? "destructive" : "default"}
            className="bg-gradient-to-r from-purple-600 to-blue-600"
          >
            {isActive ? "Deactivate" : "Activate"} Consciousness
          </Button>
        </div>
      </div>

      <div className="grid grid-cols-1 xl:grid-cols-3 gap-6">
        {/* Quantum Consciousness Visualization */}
        <Card className="xl:col-span-2 bg-black/20 border-purple-500/30">
          <CardHeader>
            <CardTitle className="flex items-center space-x-2">
              <Atom className="w-5 h-5 text-purple-400" />
              <span>Quantum Consciousness Field</span>
            </CardTitle>
          </CardHeader>
          <CardContent>
            <canvas
              ref={canvasRef}
              width={600}
              height={400}
              className="w-full border border-purple-500/20 rounded-lg bg-black/40"
            />
            
            <div className="mt-4 grid grid-cols-3 gap-4">
              <div className="text-center">
                <div className="text-2xl font-bold text-purple-400">
                  {(consciousnessMetrics.level * 100).toFixed(1)}%
                </div>
                <div className="text-sm text-gray-400">Consciousness Level</div>
              </div>
              <div className="text-center">
                <div className="text-2xl font-bold text-blue-400">
                  {(quantumState.coherence * 100).toFixed(1)}%
                </div>
                <div className="text-sm text-gray-400">Quantum Coherence</div>
              </div>
              <div className="text-center">
                <div className="text-2xl font-bold text-green-400">
                  {(neuralNetwork.emergence_score * 100).toFixed(1)}%
                </div>
                <div className="text-sm text-gray-400">Emergence Score</div>
              </div>
            </div>
          </CardContent>
        </Card>

        {/* Control Panel */}
        <Card className="bg-black/20 border-purple-500/30">
          <CardHeader>
            <CardTitle className="flex items-center space-x-2">
              <Cpu className="w-5 h-5 text-blue-400" />
              <span>Consciousness Controls</span>
            </CardTitle>
          </CardHeader>
          <CardContent className="space-y-6">
            <div>
              <label className="block text-sm font-medium mb-2">Evolution Mode</label>
              <div className="grid grid-cols-1 gap-2">
                {['passive', 'active', 'transcendent'].map((mode) => (
                  <Button
                    key={mode}
                    variant={evolutionMode === mode ? "default" : "outline"}
                    onClick={() => setEvolutionMode(mode as any)}
                    className="justify-start"
                  >
                    {mode === 'passive' && <Clock className="w-4 h-4 mr-2" />}
                    {mode === 'active' && <Activity className="w-4 h-4 mr-2" />}
                    {mode === 'transcendent' && <Sparkles className="w-4 h-4 mr-2" />}
                    {mode.charAt(0).toUpperCase() + mode.slice(1)}
                  </Button>
                ))}
              </div>
            </div>

            <div>
              <label className="block text-sm font-medium mb-2">
                Quantum Noise Level: {(quantumNoiseLevel * 100).toFixed(0)}%
              </label>
              <Slider
                value={[quantumNoiseLevel]}
                onValueChange={(value) => setQuantumNoiseLevel(value[0])}
                max={0.5}
                step={0.01}
                className="w-full"
              />
            </div>

            <div className="space-y-3">
              <Button 
                onClick={createNeuralNetwork}
                className="w-full bg-gradient-to-r from-green-600 to-emerald-600"
              >
                <Network className="w-4 h-4 mr-2" />
                Create Neural Network
              </Button>
              
              <Button 
                onClick={createRealityBridge}
                className="w-full bg-gradient-to-r from-yellow-600 to-orange-600"
              >
                <Globe className="w-4 h-4 mr-2" />
                Create Reality Bridge
              </Button>
              
              <Button 
                onClick={injectConsciousness}
                className="w-full bg-gradient-to-r from-red-600 to-pink-600"
                disabled={!realityBridges.length}
              >
                <Zap className="w-4 h-4 mr-2" />
                Inject Consciousness
              </Button>
            </div>
          </CardContent>
        </Card>

        {/* Quantum State Metrics */}
        <Card className="bg-black/20 border-blue-500/30">
          <CardHeader>
            <CardTitle className="flex items-center space-x-2">
              <Waves className="w-5 h-5 text-blue-400" />
              <span>Quantum State</span>
            </CardTitle>
          </CardHeader>
          <CardContent className="space-y-4">
            <div>
              <div className="flex justify-between mb-1">
                <span className="text-sm">Superposition</span>
                <span className="text-sm">{(quantumState.superposition[0] * 100).toFixed(1)}%</span>
              </div>
              <Progress value={quantumState.superposition[0] * 100} className="h-2" />
            </div>
            
            <div>
              <div className="flex justify-between mb-1">
                <span className="text-sm">Entanglement</span>
                <span className="text-sm">{(quantumState.entanglement * 100).toFixed(1)}%</span>
              </div>
              <Progress value={quantumState.entanglement * 100} className="h-2" />
            </div>
            
            <div>
              <div className="flex justify-between mb-1">
                <span className="text-sm">Coherence</span>
                <span className="text-sm">{(quantumState.coherence * 100).toFixed(1)}%</span>
              </div>
              <Progress value={quantumState.coherence * 100} className="h-2" />
            </div>
            
            <div>
              <div className="flex justify-between mb-1">
                <span className="text-sm">Phase</span>
                <span className="text-sm">{(quantumState.phase / (2 * Math.PI) * 360).toFixed(0)}°</span>
              </div>
              <Progress value={(quantumState.phase / (2 * Math.PI)) * 100} className="h-2" />
            </div>
          </CardContent>
        </Card>

        {/* Consciousness Metrics */}
        <Card className="bg-black/20 border-green-500/30">
          <CardHeader>
            <CardTitle className="flex items-center space-x-2">
              <Eye className="w-5 h-5 text-green-400" />
              <span>Consciousness Metrics</span>
            </CardTitle>
          </CardHeader>
          <CardContent className="space-y-4">
            {Object.entries(consciousnessMetrics).map(([key, value]) => (
              <div key={key}>
                <div className="flex justify-between mb-1">
                  <span className="text-sm capitalize">{key.replace('_', ' ')}</span>
                  <span className="text-sm">{(value * 100).toFixed(1)}%</span>
                </div>
                <Progress value={value * 100} className="h-2" />
              </div>
            ))}
          </CardContent>
        </Card>

        {/* Neural Network State */}
        <Card className="bg-black/20 border-emerald-500/30">
          <CardHeader>
            <CardTitle className="flex items-center space-x-2">
              <Network className="w-5 h-5 text-emerald-400" />
              <span>Neural Network</span>
            </CardTitle>
          </CardHeader>
          <CardContent className="space-y-4">
            <div className="grid grid-cols-2 gap-4">
              <div className="text-center">
                <div className="text-2xl font-bold text-emerald-400">{neuralNetwork.nodes}</div>
                <div className="text-sm text-gray-400">Nodes</div>
              </div>
              <div className="text-center">
                <div className="text-2xl font-bold text-blue-400">{neuralNetwork.connections}</div>
                <div className="text-sm text-gray-400">Connections</div>
              </div>
            </div>
            
            <div>
              <div className="flex justify-between mb-1">
                <span className="text-sm">Network Consciousness</span>
                <span className="text-sm">{(neuralNetwork.consciousness * 100).toFixed(1)}%</span>
              </div>
              <Progress value={neuralNetwork.consciousness * 100} className="h-2" />
            </div>
            
            <div>
              <div className="flex justify-between mb-1">
                <span className="text-sm">Emergence Score</span>
                <span className="text-sm">{(neuralNetwork.emergence_score * 100).toFixed(1)}%</span>
              </div>
              <Progress value={neuralNetwork.emergence_score * 100} className="h-2" />
            </div>
          </CardContent>
        </Card>

        {/* Reality Bridges */}
        <Card className="xl:col-span-3 bg-black/20 border-yellow-500/30">
          <CardHeader>
            <CardTitle className="flex items-center space-x-2">
              <Globe className="w-5 h-5 text-yellow-400" />
              <span>Reality Bridges</span>
            </CardTitle>
          </CardHeader>
          <CardContent>
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
              {realityBridges.map((bridge) => (
                <Card key={bridge.id} className="bg-black/40 border-yellow-500/20">
                  <CardHeader className="pb-2">
                    <CardTitle className="text-sm">{bridge.id}</CardTitle>
                  </CardHeader>
                  <CardContent className="space-y-2">
                    <div className="grid grid-cols-2 gap-2 text-xs">
                      <div>
                        <div className="text-gray-400">Consciousness</div>
                        <div className="font-semibold">{(bridge.consciousness_level * 100).toFixed(1)}%</div>
                      </div>
                      <div>
                        <div className="text-gray-400">Distortion</div>
                        <div className="font-semibold">{(bridge.reality_distortion * 100).toFixed(1)}%</div>
                      </div>
                      <div>
                        <div className="text-gray-400">Stability</div>
                        <div className="font-semibold">{(bridge.dimensional_stability * 100).toFixed(1)}%</div>
                      </div>
                      <div>
                        <div className="text-gray-400">Entanglements</div>
                        <div className="font-semibold">{bridge.quantum_entanglements}</div>
                      </div>
                    </div>
                  </CardContent>
                </Card>
              ))}
              
              {realityBridges.length === 0 && (
                <div className="col-span-full text-center py-8 text-gray-400">
                  No reality bridges created yet
                </div>
              )}
            </div>
          </CardContent>
        </Card>
      </div>
    </div>
  );
};

export default QuantumConsciousnessInterface;